package org.jerlang.erts.erlang;

import org.jerlang.type.Boolean;
import org.jerlang.type.List;
import org.jerlang.type.PID;
import org.jerlang.type.Term;

public class ErlangIsPid {

    private ErlangIsPid() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 1:
            return is_pid_1(params.head());
        default:
            throw Error.badarg;
        }
    }

    /**
     * http://www.erlang.org/doc/man/erlang.html#is_pid-1
     */
    public static Term is_pid_1(Term term) {
        return Boolean.of(term instanceof PID);
    }

}
