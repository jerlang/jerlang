package org.jerlang.erts.erlang;

import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Integer remainder of X/Y.
 */
public class ErlangRem {

    private ErlangRem() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 2:
            Integer a = params.head().toInteger();
            params = params.tail();
            Integer b = params.head().toInteger();
            return rem_2(a, b);
        default:
            throw new Error("badarg");
        }
    }

    public static Integer rem_2(Integer a, Integer b) {
        return new Integer(a.toBigInteger().remainder(b.toBigInteger()));
    }

}
