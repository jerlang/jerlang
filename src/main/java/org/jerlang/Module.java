package org.jerlang;

import java.util.HashMap;
import java.util.Map;

import org.jerlang.type.Atom;

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

    private final Atom name;
    private final Map<FunctionSignature, Object> exported_functions;

    public Module(Atom name) {
        this.name = name;
        this.exported_functions = new HashMap<>();
    }

    public Atom name() {
        return name;
    }

    public boolean hasFunction(FunctionSignature functionSignature) {
        return exported_functions.containsKey(functionSignature);
    }

}
