package org.jerlang.stdlib.io;

import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Atom;
import org.jerlang.type.List;
import org.jerlang.type.Term;

public class IOWrite {

    private IOWrite() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 1:
            return write_1(params.head().toList());
        default:
            throw Error.badarg;
        }
    }

    /**
     * Writes the term Term to the standard output (IoDevice).
     *
     * http://erlang.org/doc/man/io.html#write-1
     */
    public static Term write_1(Term term) {
        System.out.println(term);
        return Atom.of("ok");
    }

}
