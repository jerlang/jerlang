package org.jerlang.erts.erlang;

import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Str;
import org.jerlang.type.Term;

public class ErlangListToInteger {

    private ErlangListToInteger() {
    }

    public static Term dispatch(List params) {
        System.err.println(params.head());
        System.err.println("is integer: " + (params.head() instanceof Integer));
        Str string = params.head().toStr();
        switch (params.length()) {
        case 1:
            return list_to_integer_1(string);
        case 2:
            params = params.tail();
            Integer base = params.head().toInteger();
            return list_to_integer_2(string, base);
        default:
            throw new Error("badarg");
        }
    }

    /**
     * Returns an integer whose text representation in base Base is String.
     * Failure: badarg if String contains a bad representation of an integer.
     *
     * http://www.erlang.org/doc/man/erlang.html#list_to_integer-1
     */
    public static Integer list_to_integer_1(List string) {
        return list_to_integer_2(string, Integer.of(10));
    }

    /**
     * Returns an integer whose text representation in base Base is String.
     * Failure: badarg if String contains a bad representation of an integer.
     *
     * http://www.erlang.org/doc/man/erlang.html#list_to_integer-2
     */
    public static Integer list_to_integer_2(List str, Integer base) {
        return null;
    }

}
