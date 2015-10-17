package org.jerlang.erts.erlang;

import org.jerlang.type.List;
import org.jerlang.type.Term;

public class ErlangHd {

    private ErlangHd() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 1:
            return hd_1(params.head().toList());
        default:
            throw Error.badarg;
        }
    }

    /**
     * Returns the head of List, that is, the first element.
     * Allowed in guard tests.
     * Failure: `badarg` if `list` is the empty list `[]`.
     */
    public static Term hd_1(List list) {
        if (list.equals(List.nil)) {
            throw Error.badarg;
        }
        return list.head();
    }

}
