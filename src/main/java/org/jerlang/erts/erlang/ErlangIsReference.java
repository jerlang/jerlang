package org.jerlang.erts.erlang;

import org.jerlang.type.Boolean;
import org.jerlang.type.List;
import org.jerlang.type.Reference;
import org.jerlang.type.Term;

public class ErlangIsReference {

    private ErlangIsReference() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 1:
            return is_reference_1(params.head());
        default:
            throw Error.badarg;
        }
    }

    /**
     * http://www.erlang.org/doc/man/erlang.html#is_reference-1
     */
    public static Term is_reference_1(Term term) {
        return Boolean.of(term instanceof Reference);
    }

}
