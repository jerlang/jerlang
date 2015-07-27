package org.jerlang.erts.erlang;

import org.jerlang.erts.Erlang;
import org.jerlang.type.Atom;
import org.jerlang.type.List;
import org.jerlang.type.Term;

public class ErlangHd {

    private ErlangHd() {
    }

    public static Term dispatch(List params) {
        switch (Erlang.length(params).toInt()) {
        case 1:
            return hd_1(params.head().toList());
        default:
            throw new Error("badarg");
        }
    }

    /**
     * Returns the head of List, that is, the first element.
     * Allowed in guard tests.
     * Failure: `badarg` if `list` is the empty list `[]`.
     */
    public static Term hd_1(List list) {
        if (list.equals(List.nil)) {
            Erlang.error(Atom.of("badarg"));
        }
        return list.head();
    }

}
