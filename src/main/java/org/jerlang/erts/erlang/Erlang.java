package org.jerlang.erts.erlang;

import org.jerlang.type.Integer;
import org.jerlang.type.List;

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

    /**
     * Returns the length of List.
     *
     * http://www.erlang.org/doc/man/erlang.html#length-1
     */
    public static Integer length(List list) {
        if (list == List.nil) {
            return new Integer(0);
        }
        int length = 0;
        while (list.head() != null) {
            list = list.tail();
            length++;
        }
        return new Integer(length);
    }

}
