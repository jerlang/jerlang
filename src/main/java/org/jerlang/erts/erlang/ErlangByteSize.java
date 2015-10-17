package org.jerlang.erts.erlang;

import org.jerlang.type.BitString;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;

public class ErlangByteSize {

    private ErlangByteSize() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 1:
            BitString bitString = params.head().toBitString();
            return byte_size_1(bitString);
        default:
            throw Error.badarg;
        }
    }

    /**
     * Returns an integer which is the number of bytes needed to
     * contain Bitstring. That is, if the number of bits in Bitstring
     * is not divisible by 8, the resulting number of bytes will be
     * rounded up.
     *
     * http://www.erlang.org/doc/man/erlang.html#byte_size-1
     */
    public static Integer byte_size_1(BitString bitString) {
        return Integer.of(bitString.length());
    }

}
