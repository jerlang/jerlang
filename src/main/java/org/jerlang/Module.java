package org.jerlang;

import java.io.File;
import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;

import org.jerlang.erts.emulator.Instruction;
import org.jerlang.erts.erlang.Error;
import org.jerlang.stdlib.beam_lib.BeamData;
import org.jerlang.stdlib.beam_lib.BeamLibMD5;
import org.jerlang.stdlib.beam_lib.LambdaInfo;
import org.jerlang.type.Atom;
import org.jerlang.type.Binary;
import org.jerlang.type.Fun;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Str;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;
import org.jerlang.type.stack.StackFrame;
import org.jerlang.util.StringUtil;
import org.jerlang.vm.VirtualMachine;

/**
 * = Modules
 *
 * Erlang code is divided into modules.
 * A module consists of a sequence of attributes and function declarations,
 * each terminated by period (.).
 *
 * Example:
 * ----
 * -module(m).          % module attribute
 * -export([fact/1]).   % module attribute
 *
 * fact(N) when N>0 ->  % beginning of function declaration
 *     N * fact(N-1);   %  |
 * fact(0) ->           %  |
 *     1.               % end of function declaration
 * ----
 *
 * http://www.erlang.org/doc/reference_manual/modules.html
 */

public class Module {

    private static final MethodType METHOD_TYPE = MethodType.methodType(Term.class, List.class);
    private static final MethodType NAME_METHOD = MethodType.methodType(Atom.class);

    private final BeamData beamData;
    private final Class<?> moduleClass;
    private final Atom name;
    private final Map<FunctionSignature, Fun> exported_functions;
    private final java.util.Set<FunctionSignature> native_functions;
    private Map<java.lang.Integer, FunctionSignature> functionsByLabel;
    private final Map<FunctionSignature, Integer> labels;
    private File beamFile;
    private byte[] md5;

    public Module(BeamData beamData, Atom name) {
        this.beamData = beamData;
        this.exported_functions = new HashMap<>();
        this.native_functions = new java.util.HashSet<>();
        this.labels = new HashMap<>();
        this.moduleClass = null;
        this.name = name;
    }

    public Module(Class<?> moduleClass, Atom name) {
        this.beamData = null;
        this.exported_functions = new HashMap<>();
        this.native_functions = new java.util.HashSet<>();
        this.labels = new HashMap<>();
        this.moduleClass = moduleClass;
        this.name = name;
    }

    /**
     * An empty module that Java code fills with functions, as the embedding
     * API does. It has no class and no BEAM file, so only the functions that
     * are added to it can be called.
     */
    public static Module empty(Atom name) {
        return new Module((Class<?>) null, name);
    }

    /**
     * Export a Java lambda as a function of this module. It is marked as a
     * built-in, so a call runs the lambda directly instead of looking for an
     * instruction stream the module does not have.
     */
    public Module export(String name, int arity, Function<Term[], Term> body) {
        FunctionSignature signature = new FunctionSignature(this.name, Atom.of(name), Integer.of(arity));
        try {
            MethodHandle creator = MethodHandles.lookup().findStatic(Module.class, "applyJavaFunction",
                MethodType.methodType(Term.class, Function.class, List.class));
            exported_functions.put(signature, new Fun(signature, creator.bindTo(body)));
            native_functions.add(signature);
        } catch (NoSuchMethodException | IllegalAccessException e) {
            throw new IllegalStateException("could not export " + signature, e);
        }
        return this;
    }

    private static Term applyJavaFunction(Function<Term[], Term> body, List params) {
        Term[] arguments = new Term[params.length()];
        List rest = params;
        for (int index = 0; index < arguments.length; index++) {
            arguments[index] = rest.head();
            rest = rest.tail();
        }
        return body.apply(arguments);
    }

    public Term apply(FunctionSignature signature, Term params) throws Error {
        if (!hasFunction(signature)) {
            // A call to a function the module does not export, or does not
            // have at all, is what OTP reports as undef.
            throw new Error(Atom.of("undef"));
        }
        Process process = VirtualMachine.self();
        if (process == null) {
            if (native_functions.contains(signature)) {
                // A built-in implementation adopted by a BEAM module has no
                // instruction stream to start, so call it directly.
                return exported_functions.get(signature).apply(params);
            }
            // We have been probably invoked from plain Java code,
            // So we need to spawn a process and execute the function
            // within that process. The calling thread must not keep the
            // spawned process as its "self": a later call would otherwise
            // run in the finished process instead of spawning a new one.
            ProcessOrPort previous = ProcessRegistry.self();
            process = VirtualMachine.instance().spawn(signature.module(), signature.function(), params.toList());
            ProcessRegistry.self(process);
            try {
                // The scheduler thread finishes the process and counts the
                // latch down. Polling every 10 ms instead put a floor of one
                // scheduler wakeup under every call made from Java. The state
                // is not the signal: EXITING is reached inside nested calls
                // too, before the outer activation has produced its result.
                while (!process.awaitExit(1000)) {
                    // The process is still running.
                }
                Throwable failure = process.failure();
                if (failure != null) {
                    if (failure instanceof Error) {
                        throw (Error) failure;
                    }
                    if (failure instanceof RuntimeException) {
                        throw (RuntimeException) failure;
                    }
                    throw new Error(Str.of(String.valueOf(failure)));
                }
                return process.getX(0);
            } finally {
                ProcessRegistry.self(previous);
            }
        } else {
            process.pushSignature(signature);
            // A function adopted from the built-in implementation (OTP ships
            // BIFs and NIFs as nif_error stubs in its own modules) runs Java
            // code, which never executes a `return`. It must not get a stack
            // frame, because nothing would ever pop it.
            boolean instructionStream = !isInternal() && !native_functions.contains(signature);
            if (instructionStream) {
                process.pushFrame(new StackFrame(signature.module(), signature.function(),
                    signature.fun_arity().toInt()));
            }
            Fun fun = exported_functions.get(signature);
            // The callee runs as its own activation, so its continuation
            // pointer is empty: a `return` ends its instruction stream instead
            // of jumping to an index that belongs to the caller's module. The
            // caller's pointer is put back afterwards so its own tail calls
            // still know where they return to.
            int callerCP = process.getCP();
            if (instructionStream) {
                process.setCP(0);
            }
            Tracing.Pattern tracePattern = Tracing.beforeCall(process, signature, params);
            try {
                return fun.apply(params);
            } finally {
                Tracing.afterCall(process, signature, tracePattern);
                process.popSignature();
                if (instructionStream) {
                    process.setCP(callerCP);
                }
            }
        }
    }

    public BeamData beamData() {
        return beamData;
    }

    /**
     * The BEAM file this module was loaded from, or null when it is
     * implemented by the runtime.
     */
    public File beamFile() {
        return beamFile;
    }

    public void setBeamFile(File beamFile) {
        this.beamFile = beamFile;
    }

    /**
     * The 16 byte BeamLibMD5 value that NEW_FUN_EXT uses to identify the
     * module version. A module compiled from an in-memory or Java class
     * has no BEAM file, so its funs use a zero digest.
     */
    public synchronized byte[] md5() {
        if (md5 != null) {
            return md5;
        }
        md5 = new byte[16];
        if (beamData == null) {
            return md5;
        }
        try {
            Term result = BeamLibMD5.md5_1(Str.of(beamPath()));
            if (result instanceof Tuple && ((Tuple) result).arity() == 2
                && Atom.of("ok").equals(((Tuple) result).element(1))) {
                Tuple info = ((Tuple) result).element(2).toTuple();
                if (info.arity() == 2 && info.element(2) instanceof Binary) {
                    md5 = ((Binary) info.element(2)).toByteArray();
                }
            }
        } catch (Throwable e) {
            // A missing or unreadable file leaves the zero digest. Funs
            // still work locally and between JErlang nodes.
        }
        return md5;
    }

    /**
     * The base name of the source file this module was compiled from, as
     * recorded in the compile info chunk, or null if the module has none.
     */
    public Str sourceName() {
        if (beamData == null || beamData.compileInfoChunk() == null) {
            return null;
        }
        List info = beamData.compileInfoChunk().compile_info();
        while (info.length() > 0) {
            Term entry = info.head();
            if (entry instanceof Tuple && ((Tuple) entry).arity() == 2
                && ((Tuple) entry).element(1).equals(Atom.of("source"))) {
                String path = ((Tuple) entry).element(2).toStr().string();
                int separator = Math.max(path.lastIndexOf('/'), path.lastIndexOf('\\'));
                return Str.of(separator >= 0 ? path.substring(separator + 1) : path);
            }
            info = info.tail();
        }
        return null;
    }

    /**
     * The file the module digest is read from. The code path decides where a
     * module was found, so the digest must follow the same file.
     */
    private String beamPath() {
        if (beamFile != null) {
            return beamFile.getPath();
        }
        return name.toString() + ".beam";
    }

    /**
     * The function whose body starts at the given label, or null if no
     * function starts there.
     */
    public FunctionSignature functionAtLabel(int label) {
        if (beamData == null) {
            return null;
        }
        if (functionsByLabel == null) {
            functionsByLabel = new HashMap<>();
            for (FunctionSignature s : beamData.exportTableChunk().exports()) {
                functionsByLabel.put(s.label().toInt(), s);
            }
            if (beamData.localFunctionTableChunk() != null) {
                List locals = beamData.localFunctionTableChunk().locals();
                while (locals.length() > 0) {
                    Term head = locals.head();
                    if (head instanceof FunctionSignature) {
                        FunctionSignature s = (FunctionSignature) head;
                        functionsByLabel.putIfAbsent(s.label().toInt(), s);
                    }
                    locals = locals.tail();
                }
            }
            if (beamData.functionTableChunk() != null) {
                for (LambdaInfo lambda : beamData.functionTableChunk().lambdas()) {
                    functionsByLabel.putIfAbsent(lambda.label().toInt(),
                        lambda.toFunctionSignature(name));
                }
            }
        }
        return functionsByLabel.get(label);
    }

    /**
     * The label of an exported function, or null when the module does not
     * export it. erlang:make_fun/3 uses this to turn M:F/A into a fun.
     */
    public Integer exportLabel(FunctionSignature signature) {
        return labels.get(signature);
    }

    /**
     * Whether calling this function runs a BEAM instruction stream, as opposed
     * to a built-in implementation adopted by the module. Only a BEAM callee
     * is an activation of its own; a builtin runs inside its caller's frame.
     */
    public boolean hasInstructionStream(FunctionSignature signature) {
        return !isInternal() && !native_functions.contains(signature);
    }

    public void export() {
        if (moduleClass == null) {
            // The module was loaded from a BEAM file
            MethodHandle mh = null;
            try {
                mh = MethodHandles.lookup().findStatic(Interpreter.class, "dispatch", METHOD_TYPE);
            } catch (Exception e) {
                e.printStackTrace();
            }
            for (FunctionSignature s : beamData.exportTableChunk().exports()) {
                labels.put(s, s.label());
                exported_functions.put(s, new Fun(s, mh));
            }
        } else {
            // The module is included in JErlang
            for (Method method : moduleClass.getDeclaredMethods()) {
                int arity = method.getParameterCount();
                if (method.getName().startsWith("_")) {
                    // If the method name is a reserved Java keyword,
                    // the method is prefixed by an underscore.
                    // For example: `erlang:throw` or `ets:new`
                    export(method.getName().substring(1), arity);
                } else {
                    export(method.getName(), arity);
                }
            }
        }
    }

    /**
     * Use the built-in implementation of a function when the BEAM module has
     * no code for it. OTP ships BIFs and NIFs that way: the module exports the
     * function, but its body only calls erlang:nif_error/1.
     *
     * This lets a real module load from disk while the runtime still supplies
     * the primitives, instead of the built-in module shadowing the BEAM file.
     */
    public void adoptBuiltins(Module builtin) {
        for (Map.Entry<FunctionSignature, Fun> entry : builtin.exported_functions.entrySet()) {
            FunctionSignature signature = entry.getKey();
            if (!exported_functions.containsKey(signature) || isNativeStub(signature)) {
                exported_functions.put(signature, entry.getValue());
                native_functions.add(signature);
            }
        }
    }

    /**
     * True if the function's BEAM body is only the placeholder the compiler
     * emits for a BIF or NIF: move undef and call erlang:nif_error/1.
     */
    private boolean isNativeStub(FunctionSignature signature) {
        if (beamData == null || beamData.codeChunk() == null) {
            return false;
        }
        org.jerlang.type.Integer label = labels.get(signature);
        if (label == null) {
            return false;
        }
        java.lang.Integer start = beamData.codeChunk().labels().get(label.toInt());
        if (start == null) {
            return false;
        }
        java.util.List<Instruction> code = beamData.codeChunk().instructions();
        for (int index = start; index < code.size(); index++) {
            Instruction instruction = code.get(index);
            switch (instruction.opcode()) {
            case label:
            case line:
            case func_info:
            case executable_line:
            case debug_line:
            case move:
                continue;
            case call_ext:
            case call_ext_only:
            case call_ext_last:
                return callsNifError(instruction);
            default:
                return false;
            }
        }
        return false;
    }

    private boolean callsNifError(Instruction instruction) {
        Term operand = instruction.arg(1);
        if (!(operand instanceof Integer)) {
            return false;
        }
        int index = operand.toInteger().toInt();
        java.util.List<FunctionSignature> imports = beamData.importTableChunk().imports();
        if (index < 0 || index >= imports.size()) {
            return false;
        }
        FunctionSignature target = imports.get(index);
        return target.module().equals(Atom.of("erlang"))
            && target.function().equals(Atom.of("nif_error"));
    }

    public Module export(String name, int arity) {
        return export(Atom.of(name), Integer.of(arity));
    }

    public Module export(Atom name, Integer arity) {
        FunctionSignature s = new FunctionSignature(
            this.name, name, arity);
        try {
            String p = moduleClass.getPackage().getName();
            String pm = p + "." + this.name.toString();
            String cn = pm + "."
                + StringUtil.snakeToCamelCase(this.name.toString())
                + StringUtil.snakeToCamelCase(name.toString());
            Class<?> c = getClass().getClassLoader().loadClass(cn);
            MethodHandle handle = MethodHandles.lookup()
                .findStatic(c, "dispatch", METHOD_TYPE);
            s = maybe_override_name(s, c);
            exported_functions.put(s, new Fun(s, handle));
        } catch (NoSuchMethodException | IllegalAccessException | ClassNotFoundException e) {
            System.err.println("Can not export: " + s);
        }
        return this;
    }

    /**
     * This module is included in JErlang and
     * not loaded from a BEAM file.
     */
    public boolean isInternal() {
        return (moduleClass != null) && (beamData == null);
    }

    /**
     * Some functions have names that cannot be expressed in Java.
     * For example: "erlang:+/2".
     * If a function implementing class declares a static name() method,
     * its return value will be used as function name instead.
     */
    private FunctionSignature maybe_override_name(FunctionSignature s, Class<?> c) {
        try {
            MethodHandle h = MethodHandles.lookup().findStatic(c, "name", NAME_METHOD);
            Atom newName = (Atom) h.invoke();
            s = new FunctionSignature(s.module(), newName, s.element(3).toInteger());
        } catch (Throwable e) {
            // Ignore
        }
        return s;
    }

    public boolean hasFunction(FunctionSignature functionSignature) {
        return exported_functions.containsKey(functionSignature);
    }

    public Atom name() {
        return name;
    }

}
