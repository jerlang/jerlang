package org.jerlang.kernel.file;

import jnr.posix.POSIX;
import jnr.posix.POSIXFactory;

import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Atom;
import org.jerlang.type.List;
import org.jerlang.type.Str;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;

public class FileGetCwd {

    private static final Atom enotsup = Atom.of("enotsup");
    private static final Atom error = Atom.of("error");
    private static final Atom ok = Atom.of("ok");
    private static final POSIX posix;

    static {
        posix = POSIXFactory.getPOSIX();
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 0:
            return get_cwd_0();
        case 1:
            return get_cwd_1(params.head().toStr());
        default:
            throw Error.badarg;
        }
    }

    /**
     * http://www.erlang.org/doc/man/file.html#get_cwd-0
     */
    public static Term get_cwd_0() {
        return Tuple.of(ok, Str.of(posix.getcwd()));
    }

    /**
     * http://www.erlang.org/doc/man/file.html#get_cwd-1
     */
    public static Term get_cwd_1(Str drive) {
        return Tuple.of(error, enotsup);
    }

}
