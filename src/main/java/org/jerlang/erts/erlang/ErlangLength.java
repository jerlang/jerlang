package org.jerlang.erts.erlang;

import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Returns the length of List.
 *
 * http://www.erlang.org/doc/man/erlang.html#length-1
 */
public class ErlangLength {

    private ErlangLength() {
    }

    public static Term dispatch(List params) {
        switch (length_1(params).toInt()) {
        case 1:
            List list = params.head().toList();
            return length_1(list);
        default:
            throw new Error("badarg");
        }
    }

    public static Integer length_1(List list) {
        return Integer.of(list.length());
    }

}
