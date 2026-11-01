package org.jerlang.kernel.application_controller;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.Map;

import org.jerlang.CodePaths;
import org.jerlang.Process;
import org.jerlang.ProcessRegistry;
import org.jerlang.erts.Erlang;
import org.jerlang.kernel.file.TermReader;
import org.jerlang.type.Atom;
import org.jerlang.type.Boolean;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.PID;
import org.jerlang.type.Str;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;
import org.jerlang.vm.Scheduler;
import org.jerlang.vm.VirtualMachine;

/**
 * The state behind application_controller.
 *
 * OTP implements the controller as a gen_server with one application_master
 * per application. The runtime keeps the same bookkeeping in Java and calls
 * the application callback module directly: the state the callback returns
 * from start/2 is the pid of the supervision tree, and stop/1 receives that
 * pid back. The tree itself is still unmodified OTP supervisor and gen_server
 * code.
 *
 * A callback can not be called from inside a BIF: starting a supervisor parks
 * the process in proc_lib's init_ack receive, and a parked process loses the
 * Java frames of the call. Each callback therefore runs in a process of its
 * own, and the caller waits for that process through a monitor. That is also
 * what lets a callback call back into the controller, for example to read
 * application:get_env/2, without deadlocking on this class' lock.
 */
public final class Applications {

    private static final Atom OK = Atom.of("ok");
    private static final Atom ERROR = Atom.of("error");
    private static final Atom APPLICATION = Atom.of("application");
    private static final Atom KERNEL = Atom.of("kernel");
    private static final Atom STDLIB = Atom.of("stdlib");
    private static final Atom UNDEFINED = Atom.of("undefined");
    private static final Atom NORMAL = Atom.of("normal");
    private static final Atom PROCESS = Atom.of("process");
    private static final Atom DOWN = Atom.of("DOWN");
    private static final Atom SYSTEM = Atom.of("system");
    private static final Atom TERMINATE = Atom.of("terminate");
    private static final Atom SHUTDOWN = Atom.of("shutdown");

    private static final Object LOCK = new Object();

    /** A loaded application resource file. */
    public static final class Spec {

        public final Atom name;
        public final Term description;
        public final Term vsn;
        public final Term mod;
        public final Term applications;
        public final Term optionalApplications;
        public final Term includedApplications;
        public final Term startPhases;
        public final Map<Term, Term> properties;

        private Spec(Atom name, Term description, Term vsn, Term mod,
                     Term applications, Term optionalApplications,
                     Term includedApplications, Term startPhases,
                     Map<Term, Term> properties) {
            this.name = name;
            this.description = description;
            this.vsn = vsn;
            this.mod = mod;
            this.applications = applications;
            this.optionalApplications = optionalApplications;
            this.includedApplications = includedApplications;
            this.startPhases = startPhases;
            this.properties = properties;
        }

    }

    private static final class Running {

        private final Spec spec;
        private final Atom type;
        private final Term pid;

        private Running(Spec spec, Atom type, Term pid) {
            this.spec = spec;
            this.type = type;
            this.pid = pid;
        }

    }

    /** The outcome of a callback that ran in a process of its own. */
    private static final class Call {

        private final Term result;
        private final Term reason;

        private Call(Term result, Term reason) {
            this.result = result;
            this.reason = reason;
        }

        private boolean crashed() {
            return reason != null;
        }

    }

    private static final Map<Atom, Spec> loaded = new LinkedHashMap<>();
    private static final Map<Atom, Running> running = new LinkedHashMap<>();
    private static final Map<Atom, Map<Term, Term>> environment = new LinkedHashMap<>();

    static {
        reset();
    }

    private Applications() {
    }

    /**
     * Drop every loaded application and go back to the boot state, where only
     * kernel and stdlib run (the runtime itself provides them).
     */
    public static void reset() {
        synchronized (LOCK) {
            loaded.clear();
            running.clear();
            environment.clear();
            running.put(KERNEL, new Running(builtin(KERNEL, "ERTS  Kernel"),
                Atom.of("permanent"), List.nil));
            running.put(STDLIB, new Running(builtin(STDLIB, "ERTS  Standard Library"),
                Atom.of("permanent"), List.nil));
            environment.put(KERNEL, new LinkedHashMap<>());
            environment.put(STDLIB, new LinkedHashMap<>());
        }
    }

    private static Spec builtin(Atom name, String description) {
        return new Spec(name, Str.of(description), Str.of(""), List.nil, List.nil, List.nil,
            List.nil, List.nil, new LinkedHashMap<>());
    }

    /**
     * Load an application by name from the code path, or an application
     * specification given as `{application, Name, Properties}`.
     */
    public static Term load(Term application) {
        if (application instanceof Tuple) {
            return loadSpec(application.toTuple(), null);
        }
        Atom name = application.toAtom();
        synchronized (LOCK) {
            if (loaded.containsKey(name)) {
                return error(Tuple.of(Atom.of("already_loaded"), name));
            }
        }
        String fileName = name.toString() + ".app";
        String path = CodePaths.findFile(fileName);
        if (path == null) {
            return error(Tuple.of(Str.of("no such file or directory"), Str.of(fileName)));
        }
        List terms;
        try {
            String text = new String(Files.readAllBytes(Paths.get(path)), StandardCharsets.UTF_8);
            terms = TermReader.parseAll(text);
        } catch (IOException e) {
            return error(Tuple.of(Str.of("no such file or directory"), Str.of(fileName)));
        } catch (TermReader.SyntaxError e) {
            return error(Tuple.of(Integer.of(e.line()), Atom.of("erl_parse"), Str.of(e.getMessage())));
        }
        if (terms.length() != 1 || !(terms.head() instanceof Tuple)) {
            return error(Tuple.of(Atom.of("bad_application"), Str.of(fileName)));
        }
        return loadSpec((Tuple) terms.head(), name);
    }

    private static Term loadSpec(Tuple spec, Atom expected) {
        if (spec.arity() != 3 || !APPLICATION.equals(spec.element(1))
            || !(spec.element(2) instanceof Atom) || !(spec.element(3) instanceof List)) {
            return error(Tuple.of(Atom.of("bad_application"), spec));
        }
        Atom name = spec.element(2).toAtom();
        if (expected != null && !expected.equals(name)) {
            return error(Tuple.of(Atom.of("bad_application"), spec));
        }
        Spec parsed = parse(name, spec.element(3).toList());
        if (parsed == null) {
            return error(Tuple.of(Atom.of("bad_application"), spec));
        }
        synchronized (LOCK) {
            if (loaded.containsKey(name)) {
                return error(Tuple.of(Atom.of("already_loaded"), name));
            }
            loaded.put(name, parsed);
            Map<Term, Term> values = environment.get(name);
            if (values == null) {
                values = new LinkedHashMap<>();
                environment.put(name, values);
            }
            // Values from the resource file win over values set before the
            // load. Persistent set_env values are not modelled yet.
            values.putAll(envOf(spec.element(3).toList()));
        }
        return OK;
    }

    private static Spec parse(Atom name, List properties) {
        Map<Term, Term> map = new LinkedHashMap<>();
        try {
            for (Term entry : elements(properties)) {
                Tuple pair = (Tuple) entry;
                map.put(pair.element(1), pair.element(2));
            }
        } catch (RuntimeException e) {
            return null;
        }
        Term description = map.get(Atom.of("description"));
        Term vsn = map.get(Atom.of("vsn"));
        Term mod = map.get(Atom.of("mod"));
        Term applications = map.get(Atom.of("applications"));
        if (applications == null) {
            applications = List.of(KERNEL, STDLIB);
        }
        Term optional = map.get(Atom.of("optional_applications"));
        if (optional == null) {
            optional = List.nil;
        }
        Term included = map.get(Atom.of("included_applications"));
        if (included == null) {
            included = List.nil;
        }
        Term phases = map.get(Atom.of("start_phases"));
        if (phases == null) {
            phases = List.nil;
        }
        return new Spec(name,
            description == null ? Str.of("") : description,
            vsn == null ? Str.of("") : vsn,
            mod == null ? List.nil : mod,
            applications,
            optional,
            included,
            phases,
            map);
    }

    private static Map<Term, Term> envOf(List properties) {
        Map<Term, Term> env = new LinkedHashMap<>();
        Term value = null;
        for (Term entry : elements(properties)) {
            if (entry instanceof Tuple && ((Tuple) entry).arity() == 2
                && Atom.of("env").equals(((Tuple) entry).element(1))) {
                value = ((Tuple) entry).element(2);
            }
        }
        if (value == null || !(value instanceof List)) {
            return env;
        }
        for (Term entry : elements(value.toList())) {
            if (entry instanceof Tuple && ((Tuple) entry).arity() == 2) {
                env.put(((Tuple) entry).element(1), ((Tuple) entry).element(2));
            }
        }
        return env;
    }

    public static Term unload(Term application) {
        Atom name = application.toAtom();
        synchronized (LOCK) {
            if (running.containsKey(name)) {
                return error(Tuple.of(Atom.of("running"), name));
            }
            if (loaded.remove(name) == null) {
                return error(Tuple.of(Atom.of("not_loaded"), name));
            }
            environment.remove(name);
        }
        return OK;
    }

    /**
     * Start an application: load it when necessary, start its dependencies,
     * then let the callback module start its supervision tree.
     */
    public static Term start(Term application, Term type) {
        return start(application.toAtom(), type.toAtom(), new ArrayList<Atom>());
    }

    private static Term start(Atom name, Atom type, java.util.List<Atom> starting) {
        reapDead();
        Spec spec;
        synchronized (LOCK) {
            if (running.containsKey(name)) {
                return error(Tuple.of(Atom.of("already_started"), name));
            }
            spec = loaded.get(name);
        }
        if (spec == null) {
            Term result = load(name);
            if (!OK.equals(result)) {
                return result;
            }
            synchronized (LOCK) {
                spec = loaded.get(name);
            }
        }
        if (spec == null) {
            return error(Tuple.of(Atom.of("not_loaded"), name));
        }
        if (starting.contains(name)) {
            return error(Tuple.of(Atom.of("cyclic"), name));
        }
        starting.add(name);
        try {
            for (Term dependency : elements(spec.applications.toList())) {
                Atom dependencyName = dependency.toAtom();
                synchronized (LOCK) {
                    if (running.containsKey(dependencyName)) {
                        continue;
                    }
                }
                Term result = start(dependencyName, type, starting);
                if (!OK.equals(result) && !alreadyStarted(result)) {
                    return result;
                }
            }
            // An optional application that can not start does not stop the
            // application that wants it; that is what optional means.
            for (Term dependency : elements(spec.optionalApplications.toList())) {
                Atom dependencyName = dependency.toAtom();
                synchronized (LOCK) {
                    if (running.containsKey(dependencyName)) {
                        continue;
                    }
                }
                start(dependencyName, type, starting);
            }
            // Included applications belong to this application's master in
            // OTP. They are started (and stopped) with the parent here; the
            // parent callback still runs on its own, so the start arguments
            // an included application gets in OTP are not modelled.
            java.util.List<Atom> startedIncluded = new ArrayList<>();
            for (Term included : elements(spec.includedApplications.toList())) {
                Atom includedName = included.toAtom();
                synchronized (LOCK) {
                    if (running.containsKey(includedName)) {
                        continue;
                    }
                }
                Term includedResult = start(includedName, type, starting);
                if (!OK.equals(includedResult) && !alreadyStarted(includedResult)) {
                    for (Atom started : startedIncluded) {
                        stop(started);
                    }
                    return includedResult;
                }
                startedIncluded.add(includedName);
            }
            Term pid = List.nil;
            if (spec.mod instanceof Tuple && ((Tuple) spec.mod).arity() == 2) {
                Atom module = ((Tuple) spec.mod).element(1).toAtom();
                Term arguments = ((Tuple) spec.mod).element(2);
                Call call = callInProcess(module, Atom.of("start"),
                    List.of(NORMAL, arguments), 30000, true);
                if (call.crashed()) {
                    return error(call.reason);
                }
                Term result = call.result;
                if (result instanceof Tuple && ((Tuple) result).arity() >= 2
                    && OK.equals(((Tuple) result).element(1))) {
                    pid = ((Tuple) result).element(2);
                } else if (result instanceof Tuple && ((Tuple) result).arity() == 2
                    && ERROR.equals(((Tuple) result).element(1))) {
                    return result;
                } else {
                    return error(Tuple.of(Atom.of("bad_return"), result));
                }
            }
            Term phaseResult = startPhases(spec, type);
            if (!OK.equals(phaseResult)) {
                terminate(pid);
                for (Atom started : startedIncluded) {
                    stop(started);
                }
                return phaseResult;
            }
            synchronized (LOCK) {
                running.put(name, new Running(spec, type, pid));
            }
            return OK;
        } finally {
            starting.remove(name);
        }
    }

    private static boolean alreadyStarted(Term result) {
        if (!(result instanceof Tuple) || ((Tuple) result).arity() != 2
            || !ERROR.equals(((Tuple) result).element(1))) {
            return false;
        }
        Term reason = ((Tuple) result).element(2);
        return reason instanceof Tuple && ((Tuple) reason).arity() == 2
            && Atom.of("already_started").equals(((Tuple) reason).element(1));
    }

    /**
     * Stop every application that is running, newest first, so an application
     * is always stopped before the applications it was started on top of.
     * kernel and stdlib are permanent and stay, as in OTP. Used by the
     * graceful shutdown of a node; an application that fails to stop does not
     * keep the node up.
     */
    public static void stopAll() {
        java.util.List<Atom> names;
        synchronized (LOCK) {
            names = new ArrayList<>(running.keySet());
        }
        for (int index = names.size() - 1; index >= 0; index--) {
            Atom name = names.get(index);
            if (KERNEL.equals(name) || STDLIB.equals(name)) {
                continue;
            }
            try {
                stop(name);
            } catch (RuntimeException e) {
                if (java.lang.Boolean.getBoolean("jerlang.shutdown.debug")) {
                    e.printStackTrace();
                }
            }
        }
    }

    /**
     * Stop an application: let the callback module clean up, then terminate
     * the top of the supervision tree through the system protocol, which is
     * what shuts a supervisor's children down.
     */
    public static Term stop(Term application) {
        Atom name = application.toAtom();
        reapDead();
        Running current;
        synchronized (LOCK) {
            current = running.get(name);
        }
        if (current == null) {
            return error(Tuple.of(Atom.of("not_started"), name));
        }
        if (KERNEL.equals(name) || STDLIB.equals(name)) {
            return error(Tuple.of(Atom.of("permanent"), name));
        }
        if (current.spec.mod instanceof Tuple && ((Tuple) current.spec.mod).arity() == 2) {
            Atom module = ((Tuple) current.spec.mod).element(1).toAtom();
            callInProcess(module, Atom.of("stop"), List.of(current.pid), 5000, false);
        }
        terminate(current.pid);
        synchronized (LOCK) {
            running.remove(name);
        }
        for (Term included : elements(current.spec.includedApplications.toList())) {
            if (Boolean.am_true.equals(isRunning(included))) {
                stop(included);
            }
        }
        return OK;
    }

    /**
     * Run the start phases declared by the resource file, in order. OTP calls
     * the application callback's start_phase/3 after start/2 has returned,
     * and a phase that fails stops the application from starting.
     */
    private static Term startPhases(Spec spec, Atom type) {
        if (!(spec.startPhases instanceof List) || isNil(spec.startPhases)) {
            return OK;
        }
        if (!(spec.mod instanceof Tuple) || ((Tuple) spec.mod).arity() != 2) {
            return error(Tuple.of(Atom.of("bad_application"), spec.name));
        }
        for (Term entry : elements(spec.startPhases.toList())) {
            if (!(entry instanceof Tuple) || ((Tuple) entry).arity() != 2) {
                return error(Tuple.of(Atom.of("bad_application"), entry));
            }
            Tuple phase = (Tuple) entry;
            Atom module = ((Tuple) spec.mod).element(1).toAtom();
            Call call = callInProcess(module, Atom.of("start_phase"),
                List.of(phase.element(1), type, phase.element(2)), 30000, false);
            if (call.crashed()) {
                return error(Tuple.of(phase.element(1), call.reason));
            }
            if (!OK.equals(call.result)) {
                return error(Tuple.of(phase.element(1), call.result));
            }
        }
        return OK;
    }

    /**
     * Run a callback in a process of its own and wait for it to finish.
     *
     * The wait is a monitor, not a nested Erlang call: the callback may park
     * (a supervisor start waits for proc_lib's init_ack), and a parked process
     * can not return to the Java frame that called it.
     */
    private static Call callInProcess(Atom module, Atom function, List arguments, long timeout,
                                       boolean treeOutlivesCall) {
        Process caller = VirtualMachine.self();
        if (caller == null) {
            // Called from plain Java, outside any process: the ordinary apply
            // spawns a process and waits for it already.
            return new Call(Erlang.apply(module, function, arguments), null);
        }
        VirtualMachine machine = VirtualMachine.instance();
        Process child = machine.prepare(module, function, arguments);
        if (treeOutlivesCall) {
            // The callback starts a supervision tree that is linked to this
            // process. The callback returning is not the tree ending, so the
            // process' normal exit must not take the tree down.
            child.keepLinksOnNormalExit(true);
        }
        Term reference = Erlang.monitor(PROCESS, child.pid());
        // The caller's scheduler thread is about to poll for the monitor DOWN
        // message, so the callback has to run somewhere else. Mark the
        // scheduler blocked before the callback can run: the callback starts
        // its supervision tree immediately, and a descendant spawned before
        // the mark would be queued on this scheduler, behind the wait. The
        // callback would then never get its start ack and the start would
        // time out.
        Scheduler scheduler = caller.scheduler();
        if (scheduler != null) {
            scheduler.enterBlockingCall();
        }
        try {
            machine.schedule(child, scheduler);
            Term down = caller.awaitMessage(
                message -> isDown(message, reference, child.pid()), timeout);
            if (down == null) {
                Erlang.exit(child.pid(), Atom.of("kill"));
                return new Call(List.nil, Atom.of("timeout"));
            }
            Term reason = ((Tuple) down).element(5);
            if (!NORMAL.equals(reason)) {
                return new Call(List.nil, reason);
            }
            Term result = child.getX(0);
            return new Call(result == null ? List.nil : result, null);
        } finally {
            if (scheduler != null) {
                scheduler.exitBlockingCall();
            }
        }
    }

    private static boolean isDown(Term message, Term reference, PID pid) {
        return message instanceof Tuple && ((Tuple) message).arity() == 5
            && DOWN.equals(((Tuple) message).element(1))
            && reference.equals(((Tuple) message).element(2))
            && pid.equals(((Tuple) message).element(4));
    }

    /**
     * Terminate the top of a tree through the system protocol. A supervisor
     * traps exits, so exit/2 would only send it a message; the system
     * terminate request is what makes it shut its children down.
     */
    private static void terminate(Term pidTerm) {
        if (!(pidTerm instanceof PID)) {
            return;
        }
        PID pid = (PID) pidTerm;
        if (!alive(pid)) {
            return;
        }
        Process caller = VirtualMachine.self();
        if (caller == null) {
            try {
                Erlang.apply(Atom.of("sys"), TERMINATE,
                    List.of(pid, SHUTDOWN, Integer.of(5000)));
            } catch (RuntimeException e) {
                // Fall through to the brutal stop below.
            }
            if (alive(pid)) {
                Erlang.exit(pid, Atom.of("kill"));
            }
            return;
        }
        Term reference = Erlang.monitor(PROCESS, pid);
        // Mark the scheduler blocked before the message wakes the supervisor
        // or one of its children. A process woken while its scheduler is
        // blocked would sit behind this wait until the shutdown timeout.
        Scheduler scheduler = caller.scheduler();
        if (scheduler != null) {
            scheduler.enterBlockingCall();
        }
        try {
            Erlang.send(pid, Tuple.of(SYSTEM, Tuple.of(caller.pid(), reference),
                Tuple.of(TERMINATE, SHUTDOWN)));
            if (caller.awaitMessage(message -> isDown(message, reference, pid), 5000) == null
                && alive(pid)) {
                Erlang.exit(pid, Atom.of("kill"));
            }
        } finally {
            if (scheduler != null) {
                scheduler.exitBlockingCall();
            }
        }
    }

    private static boolean alive(PID pid) {
        return Boolean.am_true.equals(Erlang.is_process_alive(pid));
    }

    public static Term isRunning(Term application) {
        reapDead();
        synchronized (LOCK) {
            return Boolean.of(running.containsKey(application.toAtom()));
        }
    }

    /**
     * A supervision tree that died on its own must not stay in the controller
     * as a running application. The controller has no application_master
     * process to monitor the tree, so it checks the top pid when it is asked
     * about the running set. OTP would also restart a permanent application;
     * that is the application_master's job and is not modelled.
     */
    private static void reapDead() {
        java.util.List<Atom> dead = new ArrayList<>();
        java.util.List<Atom> included = new ArrayList<>();
        synchronized (LOCK) {
            for (Map.Entry<Atom, Running> entry : running.entrySet()) {
                Term pid = entry.getValue().pid;
                if (pid instanceof PID && !ProcessRegistry.isAlive((PID) pid)) {
                    dead.add(entry.getKey());
                }
            }
            for (Atom name : dead) {
                Running entry = running.remove(name);
                if (entry == null) {
                    continue;
                }
                // An included application is started by its parent, but its
                // supervision tree is not linked to the parent's, so it is
                // still alive when the parent dies. OTP stops it with the
                // parent; leaving it running would leak a tree whose owner no
                // longer exists.
                for (Term app : elements(entry.spec.includedApplications.toList())) {
                    if (app instanceof Atom) {
                        included.add((Atom) app);
                    }
                }
            }
        }
        // Stop them outside the lock: stopping a tree waits for processes.
        for (Atom name : included) {
            synchronized (LOCK) {
                if (!running.containsKey(name)) {
                    continue;
                }
            }
            stop(name);
        }
    }

    public static List whichApplications() {
        reapDead();
        java.util.List<Term> entries = new ArrayList<>();
        synchronized (LOCK) {
            for (Map.Entry<Atom, Running> entry : running.entrySet()) {
                Spec spec = entry.getValue().spec;
                entries.add(Tuple.of(entry.getKey(), spec.description, spec.vsn));
            }
        }
        return toList(entries);
    }

    public static List loadedApplications() {
        java.util.List<Term> entries = new ArrayList<>();
        synchronized (LOCK) {
            for (Map.Entry<Atom, Spec> entry : loaded.entrySet()) {
                Spec spec = entry.getValue();
                entries.add(Tuple.of(entry.getKey(), spec.description, spec.vsn));
            }
        }
        return toList(entries);
    }

    public static Term getMaster(Term application) {
        reapDead();
        synchronized (LOCK) {
            Running current = running.get(application.toAtom());
            if (current == null || !(current.pid instanceof PID)) {
                return UNDEFINED;
            }
            return current.pid;
        }
    }

    public static Term getApplication(Term pid) {
        synchronized (LOCK) {
            for (Map.Entry<Atom, Running> entry : running.entrySet()) {
                if (pid.equals(entry.getValue().pid)) {
                    return entry.getKey();
                }
            }
        }
        return UNDEFINED;
    }

    public static Term getApplicationModule(Term module) {
        synchronized (LOCK) {
            for (Map.Entry<Atom, Spec> entry : loaded.entrySet()) {
                Term modules = entry.getValue().properties.get(Atom.of("modules"));
                if (modules instanceof List) {
                    for (Term listed : elements((List) modules)) {
                        if (module.equals(listed)) {
                            return entry.getKey();
                        }
                    }
                }
            }
        }
        return UNDEFINED;
    }

    public static Term startType(Term pidOrName) {
        Running current = null;
        synchronized (LOCK) {
            if (pidOrName instanceof PID) {
                for (Running candidate : running.values()) {
                    if (pidOrName.equals(candidate.pid)) {
                        current = candidate;
                        break;
                    }
                }
            } else {
                current = running.get(pidOrName.toAtom());
            }
        }
        return current == null ? UNDEFINED : current.type;
    }

    public static Term getEnv(Term application, Term key) {
        synchronized (LOCK) {
            Map<Term, Term> values = environment.get(application.toAtom());
            if (values == null) {
                return UNDEFINED;
            }
            Term value = values.get(key);
            return value == null ? UNDEFINED : Tuple.of(OK, value);
        }
    }

    public static Term getEnv(Term application, Term key, Term defaultValue) {
        Term value = getEnv(application, key);
        return UNDEFINED.equals(value) ? defaultValue : ((Tuple) value).element(2);
    }

    public static List getAllEnv(Term application) {
        java.util.List<Term> entries = new ArrayList<>();
        synchronized (LOCK) {
            Map<Term, Term> values = environment.get(application.toAtom());
            if (values != null) {
                for (Map.Entry<Term, Term> entry : values.entrySet()) {
                    entries.add(Tuple.of(entry.getKey(), entry.getValue()));
                }
            }
        }
        return toList(entries);
    }

    /**
     * `set_env(Config, Opts)` where Config is `[{Application, [{Key, Val}]}]`,
     * the form application:set_env/2 uses.
     */
    public static Term setEnv(Term config, Term options) {
        for (Term entry : elements(config.toList())) {
            if (!(entry instanceof Tuple) || ((Tuple) entry).arity() != 2) {
                return error(Tuple.of(Atom.of("badarg"), entry));
            }
            Tuple pair = (Tuple) entry;
            Atom application = pair.element(1).toAtom();
            for (Term value : elements(pair.element(2).toList())) {
                if (!(value instanceof Tuple) || ((Tuple) value).arity() != 2) {
                    return error(Tuple.of(Atom.of("badarg"), value));
                }
                put(application, ((Tuple) value).element(1), ((Tuple) value).element(2));
            }
        }
        return OK;
    }

    public static Term setEnv(Term application, Term key, Term value) {
        put(application.toAtom(), key, value);
        return OK;
    }

    public static Term setEnv(Term application, Term key, Term value, Term options) {
        put(application.toAtom(), key, value);
        return OK;
    }

    private static void put(Atom application, Term key, Term value) {
        synchronized (LOCK) {
            Map<Term, Term> values = environment.get(application);
            if (values == null) {
                values = new LinkedHashMap<>();
                environment.put(application, values);
            }
            values.put(key, value);
        }
    }

    public static Term unsetEnv(Term application, Term key) {
        synchronized (LOCK) {
            Map<Term, Term> values = environment.get(application.toAtom());
            if (values != null) {
                values.remove(key);
            }
        }
        return OK;
    }

    public static Term unsetEnv(Term application, Term key, Term options) {
        return unsetEnv(application, key);
    }

    public static Term getKey(Term application, Term key) {
        synchronized (LOCK) {
            Spec spec = loaded.get(application.toAtom());
            if (spec == null) {
                return UNDEFINED;
            }
            Term value = spec.properties.get(key);
            return value == null ? UNDEFINED : Tuple.of(OK, value);
        }
    }

    public static List getAllKey(Term application) {
        java.util.List<Term> entries = new ArrayList<>();
        synchronized (LOCK) {
            Spec spec = loaded.get(application.toAtom());
            if (spec != null) {
                for (Map.Entry<Term, Term> entry : spec.properties.entrySet()) {
                    entries.add(Tuple.of(entry.getKey(), entry.getValue()));
                }
            }
        }
        return toList(entries);
    }

    public static Term info() {
        return List.nil;
    }

    public static Term permitApplication(Term application, Term permitted) {
        return Boolean.am_true;
    }

    // --------------------------------------------------------------
    // Term helpers. Kept here because every public method in the facade
    // class is exported as an Erlang function.

    private static Term error(Term reason) {
        return Tuple.of(ERROR, reason);
    }

    private static boolean isNil(Term term) {
        return term instanceof List && ((List) term).length() == 0;
    }

    static java.util.List<Term> elements(List list) {
        java.util.List<Term> terms = new ArrayList<>();
        List current = list;
        while (current != null && current.length() > 0) {
            terms.add(current.head());
            current = current.tail();
        }
        return terms;
    }

    static List toList(java.util.List<Term> terms) {
        List list = List.nil;
        for (int index = terms.size() - 1; index >= 0; index--) {
            list = new List(terms.get(index), list);
        }
        return list;
    }

}
