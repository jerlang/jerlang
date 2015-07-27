package org.jerlang.erts.erlang;

import org.jerlang.erts.Erlang;
import org.jerlang.erts.Runtime;
import org.jerlang.type.List;
import org.jerlang.type.Term;

public class ErlangGet {

    private ErlangGet() {
    }

    public static Term dispatch(List params) {
        switch (Erlang.length(params).toInt()) {
        case 0:
            return get_0();
        case 1:
            return get_1(params.head());
        default:
            throw new Error("badarg");
        }
    }

    /**
     * Returns the process dictionary as a list of `{Key, Val}` tuples.
     *
     * http://www.erlang.org/doc/man/erlang.html#get-0
     */
    public static List get_0() {
        return Runtime.getProcess().dictionary().get();
    }

    /**
     * Returns the value Val associated with Key in the process dictionary,
     * or undefined if Key does not exist.
     *
     * http://www.erlang.org/doc/man/erlang.html#get-1
     */
    public static Term get_1(Term key) {
        return Runtime.getProcess().dictionary().get(key);
    }

}
