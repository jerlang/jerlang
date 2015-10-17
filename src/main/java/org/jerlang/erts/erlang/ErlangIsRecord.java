package org.jerlang.erts.erlang;

import org.jerlang.type.Atom;
import org.jerlang.type.Boolean;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;

public class ErlangIsRecord {

    private ErlangIsRecord() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 2:
            Term term = params.head();
            params = params.tail();
            Term recordTag = params.head();
            return is_record_2(term, recordTag);
        default:
            throw Error.badarg;
        }
    }

    /**
     * http://www.erlang.org/doc/man/erlang.html#is_record-2
     */
    public static Term is_record_2(Term term, Term recordTag) {
        return Boolean.of((term instanceof Tuple)
            && (recordTag instanceof Atom)
            && recordTag.equals(term.toTuple().element(1)));
    }

}
