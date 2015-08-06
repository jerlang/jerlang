package org.jerlang;

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;

import org.jerlang.erts.erlang.Error;
import org.jerlang.stdlib.beam_lib.BeamData;
import org.jerlang.type.Atom;
import org.jerlang.type.Fun;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.jerlang.util.StringUtil;

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

    private final BeamData beamData;
    private final Class<?> moduleClass;
    private final Atom name;
    private final Map<FunctionSignature, Fun> exported_functions;
    private final Map<FunctionSignature, Integer> labels;

    public Module(BeamData beamData, Atom name) {
        this.beamData = beamData;
        this.exported_functions = new HashMap<>();
        this.labels = new HashMap<>();
        this.moduleClass = null;
        this.name = name;
    }

    public Module(Class<?> moduleClass, Atom name) {
        this.beamData = null;
        this.exported_functions = new HashMap<>();
        this.labels = new HashMap<>();
        this.moduleClass = moduleClass;
        this.name = name;
    }

    public Term apply(FunctionSignature signature, Term params) {
        if (!hasFunction(signature)) {
            throw new Error("Unknown function: " + signature);
        }
        Integer label = labels.get(signature);
        if (label == null) {
            return exported_functions.get(signature).apply(params);
        } else {
            FunctionSignature s = new FunctionSignature(
                signature.element(1).toAtom(),
                signature.element(2).toAtom(),
                signature.element(3).toInteger(),
                label);
            List extendedParams = new List(s, params.toList());
            return exported_functions.get(signature).apply(extendedParams);
        }
    }

    public BeamData beamData() {
        return beamData;
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
                export(method.getName(), method.getParameterCount());
            }
        }
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
            exported_functions.put(s, new Fun(s, handle));
        } catch (NoSuchMethodException | IllegalAccessException | ClassNotFoundException e) {
            System.err.println("Can not export: " + s);
        }
        return this;
    }

    public boolean hasFunction(FunctionSignature functionSignature) {
        return exported_functions.containsKey(functionSignature);
    }

    public Atom name() {
        return name;
    }

}
