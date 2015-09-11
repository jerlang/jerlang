package org.jerlang.stdlib.binary;

import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Binary;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;

public class BinaryLast {

    private BinaryLast() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 1:
            Binary subject = params.head().toBinary();
            return last_1(subject);
        default:
            throw new Error("badarg");
        }
    }

    /**
     * Returns the last byte of the binary Subject as an integer.
     * If the size of Subject is zero, a badarg exception is raised.
     *
     * http://www.erlang.org/doc/man/binary.html#last-1
     */
    public static Integer last_1(Binary subject) {
        return subject.at(subject.byte_length() - 1);
    }

}
