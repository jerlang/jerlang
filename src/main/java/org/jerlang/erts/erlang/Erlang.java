package org.jerlang.erts.erlang;

import org.jerlang.type.Integer;

/**
 * http://www.erlang.org/doc/man/erlang.html
 */
public class Erlang {

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
    public static Integer abs(Integer integer) {
        return new Integer(integer.toBigInteger().abs());
    }

}
