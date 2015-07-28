package org.jerlang.erts.erlang;

import org.jerlang.type.Boolean;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Prints a text representation of Term on the standard output.
 *
 * http://www.erlang.org/doc/man/erlang.html#display-1
 */
public class ErlangDisplay {

    private ErlangDisplay() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 1:
            return display_1(params.head());
        default:
            throw new Error("badarg");
        }
    }

    public static Term display_1(Term term) {
        System.out.println(term);
        return Boolean.am_true;
    }

}
