package org.jerlang.erts.erlang;

import org.jerlang.FunctionSignature;
import org.jerlang.Module;
import org.jerlang.ModuleRegistry;
import org.jerlang.type.Atom;
import org.jerlang.type.Boolean;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;

public class ErlangFunctionExported {

    private ErlangFunctionExported() {
    }

    /**
     * Returns true if the module Module is loaded and contains an exported
     * function Function/Arity, or if there is a BIF (a built-in function
     * implemented in C) with the given name; otherwise returns false.
     *
     * http://www.erlang.org/doc/man/erlang.html#function_exported-3
     */
    public static Term dispatch(List params) {
        switch (params.length()) {
        case 3:
            Atom m = params.head().toAtom();
            params = params.tail();
            Atom f = params.head().toAtom();
            params = params.tail();
            Integer a = params.head().toInteger();
            return function_exported_3(m, f, a);
        default:
            throw new Error("badarg");
        }
    }

    public static Term function_exported_3(Atom module, Atom function, Integer arity) {
        Module m = ModuleRegistry.get(module);
        if (m == null) {
            return Boolean.am_false;
        } else {
            FunctionSignature f = new FunctionSignature(module, function, arity);
            return Boolean.of(m.hasFunction(f));
        }
    }

}
