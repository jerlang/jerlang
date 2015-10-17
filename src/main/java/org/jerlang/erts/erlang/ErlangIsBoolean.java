package org.jerlang.erts.erlang;

import org.jerlang.type.Atom;
import org.jerlang.type.Boolean;
import org.jerlang.type.List;
import org.jerlang.type.Term;

public class ErlangIsBoolean {

    private ErlangIsBoolean() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 1:
            return is_boolean_1(params.head());
        default:
            throw Error.badarg;
        }
    }

    /**
     * http://www.erlang.org/doc/man/erlang.html#is_boolean-1
     */
    public static Term is_boolean_1(Term term) {
        if (term instanceof Atom) {
            switch (term.toString()) {
            case "false":
            case "true":
                return Boolean.am_true;
            }
        }
        return Boolean.am_false;
    }

}
