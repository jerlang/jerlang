package org.jerlang.erts.erlang;

import java.io.IOException;

import org.jerlang.type.BitString;
import org.jerlang.type.BitStringOutputStream;
import org.jerlang.type.List;
import org.jerlang.type.Term;

public class ErlangListToBitstring {

    private ErlangListToBitstring() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 1:
            return list_to_bitstring_1(params.head().toList());
        default:
            throw Error.badarg;
        }
    }

    /**
     * Returns a bitstring which is made from the integers and bitstrings
     * in BitstringList.
     * (The last tail in BitstringList is allowed to be a bitstring.)
     *
     * http://www.erlang.org/doc/man/erlang.html#list_to_bitstring-1
     */
    public static BitString list_to_bitstring_1(List list) {
        System.out.println("list_to_bitstring: " + list);
        try (BitStringOutputStream bsos = new BitStringOutputStream()) {
            while (list.length() > 0) {
                bsos.write(list.head());
                list = list.tail();
            }
            return bsos.toBitString();
        } catch (IOException e) {
            e.printStackTrace();
            return null;
        }
    }

}
