package org.jerlang.erts.erlang;

import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * abs(Float) -> float()
 * abs(Int) -> integer() >= 0
 *
 * Types:
 * Float = float()
 * Int = integer()
 *
 * Returns an integer or float which is the arithmetical absolute
 * value of Float or Int.
 *
 * > abs(-3.33).
 * 3.33
 * > abs(-3).
 * 3
 *
 * Allowed in guard tests.
 */
public class ErlangAbs {

    private ErlangAbs() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 1:
            return abs_1(params.head().toInteger());
        default:
            throw Error.badarg;
        }
    }

    public static Integer abs_1(Integer integer) {
        return new Integer(integer.toBigInteger().abs());
    }

}
