package org.jerlang.kernel.file;

import jnr.posix.POSIX;
import jnr.posix.POSIXFactory;

import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Atom;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Str;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;

public class FileRename {

    private static final Atom error = Atom.of("error");
    private static final Atom ok = Atom.of("ok");
    private static final POSIX posix;

    static {
        posix = POSIXFactory.getPOSIX();
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 2:
            Str src = params.head().toStr();
            params = params.tail();
            Str dst = params.head().toStr();
            return rename_2(src, dst);
        default:
            throw Error.badarg;
        }
    }

    /**
     * http://www.erlang.org/doc/man/file.html#rename-2
     */
    public static Term rename_2(Str source, Str destination) {
        int result = posix.rename(source.string(), destination.string());
        switch (result) {
        case 0:
            return ok;
        default:
            return Tuple.of(error, Integer.of(result));
        }
    }

}
