package org.jerlang.erts.erlang;

import org.jerlang.type.Boolean;
import org.jerlang.type.List;
import org.jerlang.type.Number;
import org.jerlang.type.Term;

public class ErlangIsNumber {

    private ErlangIsNumber() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 1:
            return is_number_1(params.head());
        default:
            throw new Error("badarg");
        }
    }

    /**
     * http://www.erlang.org/doc/man/erlang.html#is_number-1
     */
    public static Term is_number_1(Term term) {
        return Boolean.of(term instanceof Number);
    }

}
