package org.jerlang.erts.erlang;

import org.jerlang.type.Boolean;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;

public class ErlangIsInteger {

    private ErlangIsInteger() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 1:
            return is_integer_1(params.head());
        default:
            throw new Error("badarg");
        }
    }

    /**
     * http://www.erlang.org/doc/man/erlang.html#is_integer-1
     */
    public static Term is_integer_1(Term term) {
        return Boolean.of(term instanceof Integer);
    }

}
