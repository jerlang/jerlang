package org.jerlang.erts.erlang;

import org.jerlang.type.List;
import org.jerlang.type.Reference;
import org.jerlang.type.Term;

public class ErlangMakeRef {

    private ErlangMakeRef() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 0:
            return make_ref_0();
        default:
            throw new Error("badarg");
        }
    }

    /**
     * Return a unique reference.
     * The reference is unique among connected nodes.
     *
     * http://www.erlang.org/doc/man/erlang.html#make_ref-0
     */
    public static Reference make_ref_0() {
        return new Reference();
    }

}
