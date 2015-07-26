package org.jerlang;

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import java.util.HashMap;
import java.util.Map;

import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Atom;
import org.jerlang.type.Fun;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;

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

    private final Class<?> moduleClass;
    private final Atom name;
    private final Map<FunctionSignature, Fun> exported_functions;

    public Module(Class<?> moduleClass, Atom name) {
        this.moduleClass = moduleClass;
        this.name = name;
        this.exported_functions = new HashMap<>();
    }

    public Term apply(FunctionSignature signature, Term params) {
        if (!hasFunction(signature)) {
            throw new Error("Unknown function: " + signature);
        }
        return exported_functions.get(signature).apply(params);
    }

    public void export(String[] exports) {
        for (String export : exports) {
            String[] token = export.split("/");
            export(token[0], Integer.of(token[1]));
        }
    }

    public Module export(String name, Integer arity) {
        FunctionSignature s = new FunctionSignature(
            this.name, Atom.of(name), arity);
        try {
            String p = moduleClass.getPackage().getName();
            String pm = p + "." + this.name.toString();
            
            getClass().getClassLoader().loadClass(name)
            MethodHandle handle = MethodHandles.lookup()
                .findStatic(moduleClass, name, METHOD_TYPE);
            exported_functions.put(s, new Fun(s, handle));
        } catch (NoSuchMethodException | IllegalAccessException e) {
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
