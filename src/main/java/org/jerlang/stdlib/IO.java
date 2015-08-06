package org.jerlang.stdlib;

import org.jerlang.stdlib.io.IOFwrite;
import org.jerlang.stdlib.io.IOWrite;
import org.jerlang.type.Term;

/**
 * http://erlang.org/doc/man/io.html
 */
public class IO {

    public static Term fwrite(Term term) {
        return IOFwrite.fwrite_1(term);
    }

    public static Term write(Term term) {
        return IOWrite.write_1(term);
    }

}
