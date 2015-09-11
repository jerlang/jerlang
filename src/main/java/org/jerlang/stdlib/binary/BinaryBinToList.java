package org.jerlang.stdlib.binary;

import org.jerlang.erts.Erlang;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Binary;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;

public class BinaryBinToList {

    private BinaryBinToList() {
    }

    public static Term dispatch(List params) {
        Binary subject = params.head().toBinary();
        switch (params.length()) {
        case 1:
            return bin_to_list_1(subject);
        case 2:
            params = params.tail();
            Tuple part = params.head().toTuple();
            return bin_to_list_2(subject, part);
        case 3:
            params = params.tail();
            Integer pos = params.head().toInteger();
            params = params.tail();
            Integer len = params.head().toInteger();
            return bin_to_list_3(subject, pos, len);
        default:
            throw new Error("badarg");
        }
    }

    /**
     * The same as `bin_to_list(Subject,{0,byte_size(Subject)})`.
     *
     * http://www.erlang.org/doc/man/binary.html#bin_to_list-1
     */
    public static List bin_to_list_1(Binary subject) {
        return bin_to_list_2(subject, Tuple.of(Integer.of(0), Erlang.byte_size(subject)));
    }

    /**
     * Converts Subject to a list of byte()s,
     * each representing the value of one byte.
     * The part() denotes which part of the binary() to convert.
     *
     * http://www.erlang.org/doc/man/binary.html#bin_to_list-2
     */
    public static List bin_to_list_2(Binary subject, Tuple part) {
        int pos = part.element(1).toInteger().toInt();
        int len = part.element(1).toInteger().toInt();
        return subject.toList(pos, len);
    }

    /**
     * The same as `bin_to_list(Subject,{Pos,Len})`.
     *
     * http://www.erlang.org/doc/man/binary.html#bin_to_list-3
     */
    public static List bin_to_list_3(Binary subject, Integer pos, Integer len) {
        return bin_to_list_2(subject, Tuple.of(pos, len));
    }

}
