package org.jerlang.stdlib.binary;

import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Binary;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;

public class BinaryFirst {

    private BinaryFirst() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 1:
            Binary subject = params.head().toBinary();
            return first_1(subject);
        default:
            throw Error.badarg;
        }
    }

    /**
     * Returns the first byte of the binary Subject as an integer.
     * If the size of Subject is zero, a badarg exception is raised.
     *
     * http://www.erlang.org/doc/man/binary.html#first-1
     */
    public static Integer first_1(Binary subject) {
        return subject.at(0);
    }

}
