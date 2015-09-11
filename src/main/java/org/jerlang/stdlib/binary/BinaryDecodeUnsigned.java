package org.jerlang.stdlib.binary;

import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Atom;
import org.jerlang.type.Binary;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;

public class BinaryDecodeUnsigned {

    private static final Atom big = Atom.of("big");
    private static final Atom little = Atom.of("little");

    private BinaryDecodeUnsigned() {
    }

    public static Term dispatch(List params) {
        Binary subject = params.head().toBinary();
        switch (params.length()) {
        case 1:
            return decode_unsigned_1(subject);
        case 2:
            params = params.tail();
            Atom endianness = params.head().toAtom();
            return decode_unsigned_2(subject, endianness);
        default:
            throw new Error("badarg");
        }
    }

    /**
     * The same as `decode_unsigned(Subject, big)`.
     *
     * http://www.erlang.org/doc/man/binary.html#decode_unsigned-1
     */
    public static Integer decode_unsigned_1(Binary subject) {
        return decode_unsigned_2(subject, big);
    }

    /**
     * Converts the binary digit representation, in big or little endian,
     * of a positive integer in Subject to an Erlang integer().
     *
     * http://www.erlang.org/doc/man/binary.html#decode_unsigned-2
     */
    public static Integer decode_unsigned_2(Binary subject, Atom endianness) {
        if (big.equals(endianness)) {
            return subject.decode_unsigned_big();
        } else if (little.equals(endianness)) {
            return subject.decode_unsigned_little();
        } else {
            throw new Error("badarg");
        }
    }

}
