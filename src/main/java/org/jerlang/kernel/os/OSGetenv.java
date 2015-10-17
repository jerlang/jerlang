package org.jerlang.kernel.os;

import jnr.ffi.Pointer;
import jnr.posix.POSIX;
import jnr.posix.POSIXFactory;

import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Atom;
import org.jerlang.type.List;
import org.jerlang.type.Str;
import org.jerlang.type.Term;

import com.kenai.jffi.MemoryIO;

public class OSGetenv {

    static {
        posix = POSIXFactory.getPOSIX();
    }

    private static final POSIX posix;

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 0:
            return getenv_0();
        case 1:
            return getenv_1(params.head());
        case 2:
            Term varName = params.head();
            params = params.tail();
            Term defaultValue = params.head();
            return getenv_2(varName, defaultValue);
        default:
            throw Error.badarg;
        }
    }

    /**
     * Returns a list of all environment variables.
     * Each environment variable is given as a single string on the format
     * "VarName=Value",
     * where VarName is the name of the variable and Value its value.
     *
     * http://www.erlang.org/doc/man/os.html#getenv-0
     */
    public static List getenv_0() {
        Pointer env = posix.environ();
        List result = List.nil;
        int offset = 0;
        while (env.getPointer(offset) != null) {
            Pointer entryPointer = env.getPointer(offset);
            String entry = new String(MemoryIO.getInstance().getZeroTerminatedByteArray(entryPointer.address()));
            result = new List(Str.of(entry), result);
            offset += jnr.ffi.Runtime.getSystemRuntime().addressSize();
        }
        return result;
    }

    /**
     * Returns the Value of the environment variable VarName,
     * or false if the environment variable is undefined.
     *
     * http://www.erlang.org/doc/man/os.html#getenv-1
     */
    public static Term getenv_1(Term varName) {
        return getenv_2(varName, Atom.of("false"));
    }

    /**
     * Returns the Value of the environment variable VarName,
     * or DefaultValue if the environment variable is undefined.
     *
     * http://www.erlang.org/doc/man/os.html#getenv-2
     */
    public static Term getenv_2(Term varName, Term defaultValue) {
        String result = posix.getenv(((Str) varName).string());
        if (result == null) {
            return defaultValue;
        } else {
            return Str.of(result);
        }
    }

}
