package org.jerlang.erts.erlang;

import org.jerlang.erts.Erlang;
import org.jerlang.type.Boolean;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Prints a text representation of Term on the standard output.
 *
 * http://www.erlang.org/doc/man/erlang.html#display-1
 */
public class ErlangDisplay {

    public static Term dispatch(List params) {
        switch (Erlang.length_1(params).toInt()) {
        case 1:
            return Boolean.of(display_1(params.head()));
        default:
            throw new Error("badarg");
        }
    }

    public static boolean display_1(Term term) {
        System.out.println(term);
        return true;
    }

}
