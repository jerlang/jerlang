package org.jerlang.erts.erlang;

import org.jerlang.type.Binary;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;

public class ErlangBinaryPart {

    private ErlangBinaryPart() {
    }

    public static Term dispatch(List params) {
        Binary subject = params.head().toBinary();
        switch (params.length()) {
        case 2:
            params = params.tail();
            Tuple posLen = params.head().toTuple();
            return binary_part_2(subject, posLen);
        case 3:
            params = params.tail();
            Integer start = params.head().toInteger();
            params = params.tail();
            Integer length = params.head().toInteger();
            return binary_part_3(subject, start, length);
        default:
            throw Error.badarg;
        }
    }

    /**
     * Extracts the part of the binary described by PosLen.
     *
     * Negative length can be used to extract bytes at the end of a binary:
     *
     * 1> Bin = <<1,2,3,4,5,6,7,8,9,10>>.
     * 2> binary_part(Bin,{byte_size(Bin), -5}).
     * <<6,7,8,9,10>>
     *
     * If PosLen in any way references outside the binary,
     * a badarg exception is raised.
     *
     * Start is zero-based, i.e.:
     *
     * 1> Bin = <<1,2,3>>
     * 2> binary_part(Bin,{0,2}).
     * <<1,2>>
     *
     * See the STDLIB module binary for details about the PosLen semantics.
     *
     * Allowed in guard tests.
     *
     * http://www.erlang.org/doc/man/erlang.html#binary_part-2
     */
    public static Binary binary_part_2(Binary subject, Tuple posLen) {
        Integer start = posLen.element(1).toInteger();
        Integer length = posLen.element(2).toInteger();
        return binary_part_3(subject, start, length);
    }

    /**
     * The same as binary_part(Subject, {Start, Length}).
     * Allowed in guard tests.
     *
     * http://www.erlang.org/doc/man/erlang.html#binary_part-3
     */
    public static Binary binary_part_3(Binary subject, Integer start, Integer length) {
        return subject.part(start.toInt(), length.toInt());
    }

}
