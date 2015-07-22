package org.jerlang.kernel;

import jnr.ffi.Pointer;
import jnr.posix.POSIX;
import jnr.posix.POSIXFactory;

import org.jerlang.ModuleRegistry;
import org.jerlang.type.Atom;
import org.jerlang.type.List;
import org.jerlang.type.Str;
import org.jerlang.type.Term;

import com.kenai.jffi.MemoryIO;

/**
 * The functions in this module are operating system specific.
 * Careless use of these functions will result in programs that
 * will only run on a specific platform.
 *
 * On the other hand, with careful use these functions can be of help
 * in enabling a program to run on most platforms.
 *
 * http://www.erlang.org/doc/man/os.html
 */
public class OS {

    static {
        ModuleRegistry.register("os");
        posix = POSIXFactory.getPOSIX();
    }

    private static final POSIX posix;

    /**
     * Returns a list of all environment variables.
     * Each environment variable is given as a single string on the format
     * "VarName=Value",
     * where VarName is the name of the variable and Value its value.
     *
     * http://www.erlang.org/doc/man/os.html#getenv-0
     */
    public static List getenv() {
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
    public static Term getenv(Term varName) {
        return getenv(varName, Atom.of("false"));
    }

    /**
     * Returns the Value of the environment variable VarName,
     * or DefaultValue if the environment variable is undefined.
     *
     * http://www.erlang.org/doc/man/os.html#getenv-2
     */
    public static Term getenv(Term varName, Term defaultValue) {
        String result = posix.getenv(((Str) varName).string());
        if (result == null) {
            return defaultValue;
        } else {
            return Str.of(result);
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
    public static Str getpid() {
        return Str.of(String.valueOf(posix.getpid()));
    }

    /**
     * Sets a new Value for the environment variable VarName.
     *
     * http://www.erlang.org/doc/man/os.html#putenv-2
     */
    public static void putenv(Str varName, Str value) {
        posix.setenv(varName.string(), value.string(), 1);
    }

}
