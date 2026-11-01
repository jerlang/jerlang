package org.jerlang.vm;

import java.util.concurrent.atomic.AtomicInteger;

import org.jerlang.Process;
import org.jerlang.ProcessOrPort;
import org.jerlang.ProcessRegistry;
import org.jerlang.Tracing;
import org.jerlang.erts.erlang.Error;
import org.jerlang.jvm.HandleTable;
import org.jerlang.type.Atom;
import org.jerlang.type.Fun;
import org.jerlang.type.List;
import org.jerlang.type.PID;
import org.jerlang.type.PortID;
import org.jerlang.type.Term;
import org.jerlang.type.TimerReference;
import org.jerlang.type.Tuple;

/**
 *
 */
public class VirtualMachine {

    private Scheduler[] schedulers;
    private int nextScheduler = 0;

    private static final AtomicInteger nextPID = new AtomicInteger();

    /** One wheel owns every timer in this VM. */
    private volatile TimerWheel timerWheel;

    /** When the schedulers were last started, for the metrics uptime. */
    private volatile long startedAtMillis = 0;

    /**
     * The Java objects Erlang code holds through object handles. The table
     * belongs to this VM, not to the bridge, so a handle can not outlive its
     * node.
     */
    private final HandleTable handleTable = new HandleTable();

    // Identity of this node. A node starts out unnamed, exactly like an
    // Erlang node that was not started with -name or -sname.
    private volatile Atom node = Atom.of("nonode@nohost");
    private volatile int nodeCreation = 0;
    private volatile boolean hidden = false;

    private static final VirtualMachine VIRTUAL_MACHINE = new VirtualMachine();

    private VirtualMachine() {
        int count = schedulerCount();
        schedulers = new Scheduler[count];
        for (int index = 0; index < count; index++) {
            schedulers[index] = new Scheduler();
        }
        timerWheel = new TimerWheel();
    }

    /**
     * At least two schedulers, even in a one-CPU container. A BIF that waits
     * for another process (application start, RPC) blocks its scheduler
     * thread; with a single scheduler the process it waits for can never run.
     */
    private static int schedulerCount() {
        return Math.max(2, Runtime.getRuntime().availableProcessors());
    }

    public Term cancel(TimerReference timerReference) {
        TimerWheel.Handle handle = timerReference.handle();
        if (handle != null) {
            handle.cancel();
        }
        return Tuple.of(Atom.of("ok"), Atom.of("cancel"));
    }

    /**
     * The wheel every timer in this VM is scheduled on.
     */
    public TimerWheel timerWheel() {
        return timerWheel;
    }

    public static Process self() {
        return (Process) ProcessRegistry.self();
    }

    public void start() {
        start(schedulerCount());
    }

    /**
     * Start with an explicit number of schedulers. The default follows the
     * host's processor count; an embedding deployment may want a fixed pool,
     * and a scheduling test needs a deterministic one.
     */
    public void start(int count) {
        if (count < 1) {
            throw new IllegalArgumentException("scheduler count must be positive");
        }
        handleTable.clear();
        schedulers = new Scheduler[count];
        for (int index = 0; index < count; index++) {
            schedulers[index] = new Scheduler();
            schedulers[index].start();
        }
        TimerWheel previous = timerWheel;
        timerWheel = new TimerWheel();
        if (previous != null) {
            previous.stop();
            previous.awaitStop();
        }
        ProcessRegistry.instance().cleanup();
        org.jerlang.erts.dist.Distribution.start();
        startedAtMillis = System.currentTimeMillis();
    }

    public void shutdown() {
        org.jerlang.erts.dist.Distribution.shutdown();
        // Applications stop while the schedulers can still run their
        // supervision trees, newest first, so a tree is always gone before the
        // applications it depends on.
        org.jerlang.kernel.application_controller.Applications.stopAll();
        drain();
        for (Scheduler scheduler : schedulers) {
            scheduler.interrupt();
        }
        for (Scheduler scheduler : schedulers) {
            try {
                scheduler.join();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
        TimerWheel wheel = timerWheel;
        if (wheel != null) {
            wheel.stop();
            wheel.awaitStop();
        }
        ProcessRegistry.instance().cleanup();
        handleTable.clear();
        startedAtMillis = 0;
    }

    /**
     * Let the work that is already queued finish before the schedulers stop.
     * Bounded by jerlang.shutdown.drain.ms (default 1000): a process that
     * never finishes must not keep the JVM up.
     */
    private void drain() {
        long timeout = Math.max(0, Long.getLong("jerlang.shutdown.drain.ms", 1000L));
        long deadline = System.currentTimeMillis() + timeout;
        while (queuedWork() > 0 && System.currentTimeMillis() < deadline) {
            try {
                Thread.sleep(5);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                return;
            }
        }
    }

    /**
     * Processes and ports waiting on any scheduler.
     */
    private long queuedWork() {
        long queued = 0;
        for (Scheduler scheduler : schedulers) {
            queued += scheduler.queueDepth();
        }
        return queued;
    }

    /**
     * The schedulers of this VM, for the metrics snapshot.
     */
    public Scheduler[] schedulers() {
        return schedulers.clone();
    }

    /**
     * Milliseconds since this VM was started, or 0 while it is not running.
     */
    public long startedAtMillis() {
        return startedAtMillis;
    }

    /**
     * One reading of the runtime counters, see {@link Metrics}.
     */
    public Metrics metrics() {
        return Metrics.snapshot();
    }

    /**
     * The object handles of this node. A handle is only valid while the node
     * that created it is running; start and shutdown both drain the table.
     */
    public HandleTable handleTable() {
        return handleTable;
    }

    public static VirtualMachine instance() {
        return VIRTUAL_MACHINE;
    }

    /**
     * The name of this node, or `nonode@nohost` if it has no name yet.
     */
    public Atom node() {
        return node;
    }

    /**
     * The creation number of this node. It changes when the node is
     * restarted with the same name, so that stale PIDs and references
     * from the previous incarnation do not match the new one.
     */
    public int nodeCreation() {
        return nodeCreation;
    }

    /**
     * Name this node, as -name and -sname do.
     */
    public void setNode(Atom node) {
        this.node = node;
    }

    public void setNode(Atom node, int creation) {
        this.node = node;
        this.nodeCreation = creation;
    }

    /**
     * Whether this node is hidden, as -hidden and net_kernel:start/2
     * declare it. A hidden node does not announce itself as published.
     */
    public boolean isHidden() {
        return hidden;
    }

    public void setHidden(boolean hidden) {
        this.hidden = hidden;
    }

    public Process resolve(PID pid) {
        return (Process) ProcessRegistry.resolve(pid);
    }

    public TimerReference send_after(int time, Term message, PID pid) {
        TimerWheel.Handle handle = timerWheel.schedule(() -> {
            Process target = resolve(pid);
            if (target != null) {
                target.send(message);
            }
        }, time);
        return new TimerReference(handle);
    }

    /**
     * erlang:start_timer/3,4: the receiver gets {timeout, TRef, Message}. The
     * reference has to exist before the timer is scheduled, because it is part
     * of the message, so it is created around the scheduled task.
     */
    public TimerReference start_timer(int time, Term message, PID pid) {
        TimerReference reference = new TimerReference(null);
        TimerWheel.Handle handle = timerWheel.schedule(() -> {
            Process target = resolve(pid);
            if (target != null) {
                target.send(Tuple.of(Atom.of("timeout"), reference, message));
            }
        }, time);
        reference.setHandle(handle);
        return reference;
    }

    public Process spawn(Fun fun) {
        Process process = prepare(fun);
        schedule(process);
        return process;
    }

    public Process spawn(Atom module, Atom fun, List args) {
        Process process = prepare(module, fun, args);
        schedule(process);
        return process;
    }

    /**
     * Create a process without putting it on a run queue. A remote spawn
     * uses this to send its SPAWN_REPLY, with the link and monitor already
     * in place, before the new process can run and terminate.
     */
    public Process prepare(Fun fun) {
        PID pid = new PID(node, nextPID.incrementAndGet(), 0, nodeCreation);
        Process process = new Process(pid, fun);
        ProcessRegistry.register(process);
        Tracing.applyToNewProcess(process);
        return process;
    }

    public Process prepare(Atom module, Atom fun, List args) {
        PID pid = new PID(node, nextPID.incrementAndGet(), 0, nodeCreation);
        Process process = new Process(pid, module, fun, args);
        ProcessRegistry.register(process);
        Tracing.applyToNewProcess(process);
        return process;
    }

    /**
     * Put a prepared process on a run queue.
     */
    public void schedule(Process process) {
        schedule(process, null);
    }

    /**
     * Put a process that was woken from a wait back on a run queue.
     *
     * Normally the process keeps the scheduler it last ran on. That scheduler
     * may however be blocked in a wait for this very process (application
     * start and stop do that); queueing behind the wait would deadlock until
     * the timeout, so in that case the wakeup goes to another scheduler.
     */
    public void wake(ProcessOrPort process) {
        Scheduler current = process.scheduler();
        if (current != null && !current.isInBlockingCall()) {
            current.add(process);
        } else {
            schedulers[nextAvailableScheduler(null)].add(process);
        }
    }

    /**
     * Put a prepared process on a run queue, avoiding a scheduler whose thread
     * is blocked waiting for that process. Callers that block (application
     * start, RPC, distribution) must schedule the process they wait for away
     * from their own scheduler.
     */
    public void schedule(Process process, Scheduler avoid) {
        schedulers[nextAvailableScheduler(avoid)].add(process);
    }

    private synchronized int nextAvailableScheduler(Scheduler avoid) {
        int count = schedulers.length;
        for (int offset = 0; offset < count; offset++) {
            int index = (nextScheduler + offset) % count;
            Scheduler candidate = schedulers[index];
            if (candidate != avoid && !candidate.isInBlockingCall()) {
                nextScheduler = (index + 1) % count;
                return index;
            }
        }
        // Every scheduler is blocked. Round-robin anyway: the process has to
        // have a queue, and one of the waiting calls may return without it.
        int index = nextScheduler % count;
        nextScheduler = (index + 1) % count;
        return index;
    }

    /**
     * A PID that names no process. erlang:spawn/4 returns one when the
     * target node can not be reached, like Erlang/OTP.
     */
    public PID uselessPid() {
        return new PID(node, nextPID.incrementAndGet(), 0, nodeCreation);
    }

    public PortID spawn_port(Term command, List portSettings) {
        throw new Error("not implemented");
    }

    public PortID spawn_driver_port(Term command, List portSettings) {
        throw new Error("not implemented");
    }

    public PortID spawn_executable_port(Term fileName, List portSettings) {
        throw new Error("not implemented");
    }

}
