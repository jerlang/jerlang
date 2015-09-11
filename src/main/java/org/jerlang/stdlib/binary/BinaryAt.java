package org.jerlang.stdlib.binary;

import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Binary;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;

public class BinaryAt {

    private BinaryAt() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 2:
            Binary subject = params.head().toBinary();
            params = params.tail();
            Integer pos = params.head().toInteger();
            return at_2(subject, pos);
        default:
            throw new Error("badarg");
        }
    }

    /**
     * Returns the byte at position Pos (zero-based) in the binary Subject
     * as an integer. If Pos >= byte_size(Subject), a badarg exception is
     * raised.
     *
     * http://www.erlang.org/doc/man/binary.html#at-2
     */
    public static Integer at_2(Binary subject, Integer pos) {
        return subject.at(pos.toInt());
    }

}
