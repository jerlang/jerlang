package org.jerlang.erts.erlang;

import org.jerlang.erts.Erlang;
import org.jerlang.type.Atom;
import org.jerlang.type.List;
import org.jerlang.type.Term;

public class ErlangTl {

    private ErlangTl() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 1:
            return tl_1(params.head().toList());
        default:
            throw Error.badarg;
        }
    }

    /**
     * Returns the tail of List, that is, the list minus the first element.
     * Allowed in guard tests.
     * Failure: `badarg` if `list` is the empty list `[]`.
     *
     * http://www.erlang.org/doc/man/erlang.html#tl-1
     */
    public static Term tl_1(List list) {
        if (list.equals(List.nil)) {
            Erlang.error(Atom.of("badarg"));
        }
        return list.tail();
    }

}
