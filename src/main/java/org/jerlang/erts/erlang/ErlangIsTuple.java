package org.jerlang.erts.erlang;

import org.jerlang.type.Boolean;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;

public class ErlangIsTuple {

    private ErlangIsTuple() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 1:
            return is_tuple_1(params.head());
        default:
            throw Error.badarg;
        }
    }

    /**
     * http://www.erlang.org/doc/man/erlang.html#is_tuple-1
     */
    public static Term is_tuple_1(Term term) {
        return Boolean.of(term instanceof Tuple);
    }

}
