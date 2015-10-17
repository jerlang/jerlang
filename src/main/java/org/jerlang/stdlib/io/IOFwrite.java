package org.jerlang.stdlib.io;

import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Atom;
import org.jerlang.type.List;
import org.jerlang.type.Str;
import org.jerlang.type.Term;

public class IOFwrite {

    private IOFwrite() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 1:
            return fwrite_1(params.head().toList());
        default:
            throw Error.badarg;
        }
    }

    /**
     * Writes the term Term to the standard output (IoDevice).
     *
     * http://erlang.org/doc/man/io.html#fwrite-1
     */
    public static Term fwrite_1(Term term) {
        if (term instanceof Str) {
            System.out.println(term.toStr().string());
        } else {
            System.out.println(term);
        }
        return Atom.of("ok");
    }

}
