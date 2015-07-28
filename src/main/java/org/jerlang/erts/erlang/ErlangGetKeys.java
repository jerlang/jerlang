package org.jerlang.erts.erlang;

import org.jerlang.erts.Runtime;
import org.jerlang.type.List;
import org.jerlang.type.Term;

public class ErlangGetKeys {

    private ErlangGetKeys() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 0:
            return get_keys_0();
        case 1:
            return get_keys_1(params.head());
        default:
            throw new Error("badarg");
        }
    }

    /**
     * Returns a list of keys all keys present in the process dictionary.
     *
     * http://www.erlang.org/doc/man/erlang.html#get_keys-0
     */
    public static List get_keys_0() {
        return Runtime.getProcess().dictionary().get_keys();
    }

    /**
     * Returns a list of keys which are associated with the value Val
     * in the process dictionary.
     *
     * http://www.erlang.org/doc/man/erlang.html#get_keys-1
     */
    public static List get_keys_1(Term value) {
        return Runtime.getProcess().dictionary().get_keys(value);
    }

}
