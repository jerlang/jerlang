package org.jerlang.kernel.os;

import jnr.posix.POSIX;
import jnr.posix.POSIXFactory;

import org.jerlang.erts.erlang.Error;
import org.jerlang.type.List;
import org.jerlang.type.Str;
import org.jerlang.type.Term;

public class OSGetpid {

    static {
        posix = POSIXFactory.getPOSIX();
    }

    private static final POSIX posix;

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 0:
            return getpid_0();
        default:
            throw Error.badarg;
        }
    }

    /**
     * Returns the process identifier of the current Erlang emulator in the
     * format most commonly used by the operating system environment.
     * Value is returned as a string containing the (usually) numerical
     * identifier for a process.
     *
     * http://www.erlang.org/doc/man/os.html#getpid-0
     */
    public static Str getpid_0() {
        return Str.of(String.valueOf(posix.getpid()));
    }

}
