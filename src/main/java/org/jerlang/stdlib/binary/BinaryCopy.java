package org.jerlang.stdlib.binary;

import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Binary;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;

public class BinaryCopy {

    private BinaryCopy() {
    }

    public static Term dispatch(List params) {
        Binary subject = params.head().toBinary();
        switch (params.length()) {
        case 1:
            return copy_1(subject);
        case 2:
            params = params.tail();
            Integer n = params.head().toInteger();
            return copy_2(subject, n);
        default:
            throw Error.badarg;
        }
    }

    /**
     * The same as `copy(Subject, 1)`.
     *
     * http://www.erlang.org/doc/man/binary.html#copy-1
     */
    public static Binary copy_1(Binary subject) {
        return copy_2(subject, Integer.ONE);
    }

    /**
     * Creates a binary with the content of Subject duplicated N times.
     *
     * This function will always create a new binary, even if N = 1.
     * By using copy/1 on a binary referencing a larger binary,
     * one might free up the larger binary for garbage collection.
     *
     * http://www.erlang.org/doc/man/binary.html#copy-2
     */
    public static Binary copy_2(Binary subject, Integer n) {
        return subject.copy(n.toInt());
    }

}
