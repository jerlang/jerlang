package org.jerlang.erts.erlang;

import org.jerlang.type.BitString;
import org.jerlang.type.List;
import org.jerlang.type.Term;

public class ErlangBitstringToList {

    private ErlangBitstringToList() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 1:
            BitString bitString = params.head().toBitString();
            return bitstring_to_list_1(bitString);
        default:
            throw Error.badarg;
        }
    }

    /**
     * Returns a list of integers which correspond to the bytes of Bitstring.
     * If the number of bits in the binary is not divisible by 8,
     * the last element of the list will be a bitstring containing
     * the remaining bits (1 up to 7 bits).
     *
     * http://www.erlang.org/doc/man/erlang.html#bitstring_to_list-1
     */
    public static List bitstring_to_list_1(BitString bitString) {
        return bitString.convert_to_list();
    }

}
