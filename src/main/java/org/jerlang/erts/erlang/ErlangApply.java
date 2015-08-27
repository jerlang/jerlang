package org.jerlang.erts.erlang;

import org.jerlang.FunctionSignature;
import org.jerlang.Module;
import org.jerlang.ModuleRegistry;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Str;
import org.jerlang.type.Term;

/**
 * Returns the result of applying Function in Module to Args.
 * The applied function must be exported from Module.
 * The arity of the function is the length of Args.
 *
 * http://www.erlang.org/doc/man/erlang.html#apply-3
 */
public class ErlangApply {

    private ErlangApply() {
    }

    public static Term dispatch(List params) throws Error {
        switch (params.length()) {
        case 3:
            Term m = params.head();
            params = params.tail();
            Term f = params.head();
            params = params.tail();
            Term a = params.head();
            return apply_3(m, f, a);
        default:
            throw new Error("badarg");
        }
    }

    public static Term apply_3(Term m, Term f, Term a) throws Error {
        Module module = ModuleRegistry.get(m.toAtom());
        if (module == null) {
            throw new Error(Str.of("Invalid module: " + m));
        }
        FunctionSignature signature = new FunctionSignature(m.toAtom(), f.toAtom(), Integer.of(a.toList().length()));
        if (!module.hasFunction(signature)) {
            throw new Error(Str.of("Invalid function: " + signature));
        }
        return module.apply(signature, a);
    }

}
