package org.jerlang.erts.erlang;

import org.jerlang.erts.Erlang;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Str;
import org.jerlang.type.Term;

public class ErlangIntegerToList {

    private ErlangIntegerToList() {
    }

    public static Term dispatch(List params) {
        switch (Erlang.length(params).toInt()) {
        case 1:
            return integer_to_list_1(params.head().toInteger());
        case 2:
            Integer integer = params.head().toInteger();
            params = params.tail();
            Integer base = params.head().toInteger();
            return integer_to_list_2(integer, base);
        default:
            throw new Error("badarg");
        }
    }

    /**
     * Returns a string which corresponds to the text representation of Integer.
     *
     * http://www.erlang.org/doc/man/erlang.html#integer_to_list-1
     */
    public static List integer_to_list_1(Integer integer) {
        return new Str(integer.toString());
    }

    /**
     * Returns a string which corresponds to the text representation of Integer
     * in base Base.
     *
     * http://www.erlang.org/doc/man/erlang.html#integer_to_list-2
     */

    public static List integer_to_list_2(Integer integer, Integer base) {
        return new Str(integer.toBigInteger().toString(base.toInt()));
    }

}
