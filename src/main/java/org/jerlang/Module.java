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

    public Module(BeamData beamData, Atom name) {
        this.beamData = beamData;
        this.exported_functions = new HashMap<>();
        this.moduleClass = null;
        this.name = name;
    }

    public Module(Class<?> moduleClass, Atom name) {
        this.beamData = null;
        this.exported_functions = new HashMap<>();
        this.moduleClass = moduleClass;
        this.name = name;
    }

    public Term apply(FunctionSignature signature, Term params) {
        if (!hasFunction(signature)) {
            throw new Error("Unknown function: " + signature);
        }
        return exported_functions.get(signature).apply(params);
    }

    public BeamData beamData() {
        return beamData;
    }

    public void export() {
        if (moduleClass == null) {
            // The module was loaded from a BEAM file
            for (FunctionSignature s : beamData.exportTableChunk().exports()) {
                // TODO: provide link to code
                exported_functions.put(s, new Fun(s, null));
            }
        } else {
            // The module is included in jerlang
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
