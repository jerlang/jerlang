package org.jerlang.erts.erlang;

import static java.nio.charset.StandardCharsets.ISO_8859_1;

import org.jerlang.type.Binary;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;

public class ErlangIntegerToBinary {

    private ErlangIntegerToBinary() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 1:
            return integer_to_binary_1(params.head().toInteger());
        case 2:
            Integer integer = params.head().toInteger();
            params = params.tail();
            Integer base = params.head().toInteger();
            return integer_to_binary_2(integer, base);
        default:
            throw new Error("badarg");
        }
    }

    /**
     * Returns a binary which corresponds to the text representation of Integer.
     *
     * http://www.erlang.org/doc/man/erlang.html#integer_to_binary-1
     */
    public static Binary integer_to_binary_1(Integer integer) {
        return new Binary(integer.toString().getBytes(ISO_8859_1));
    }

    /**
     * http://www.erlang.org/doc/man/erlang.html#integer_to_binary-2
     */
    public static Binary integer_to_binary_2(Integer integer, Integer base) {
        return new Binary(integer.toBigInteger().toString(base.toInt()).getBytes(ISO_8859_1));
    }

}
