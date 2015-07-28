package org.jerlang.kernel.os;

import jnr.posix.POSIX;
import jnr.posix.POSIXFactory;

import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Boolean;
import org.jerlang.type.List;
import org.jerlang.type.Str;
import org.jerlang.type.Term;

public class OSPutenv {

    private static final POSIX posix;

    static {
        posix = POSIXFactory.getPOSIX();
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 2:
            Str varName = params.head().toStr();
            params = params.tail();
            Str value = params.head().toStr();
            return putenv_2(varName, value);
        default:
            throw new Error("badarg");
        }
    }

    /**
     * Sets a new Value for the environment variable VarName.
     *
     * http://www.erlang.org/doc/man/os.html#putenv-2
     */
    public static Term putenv_2(Str varName, Str value) {
        posix.setenv(varName.string(), value.string(), 1);
        return Boolean.am_true;
    }

}
