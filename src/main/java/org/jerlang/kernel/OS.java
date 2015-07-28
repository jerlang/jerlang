package org.jerlang.kernel;

import org.jerlang.kernel.os.OSGetenv;
import org.jerlang.kernel.os.OSGetpid;
import org.jerlang.kernel.os.OSPutenv;
import org.jerlang.type.List;
import org.jerlang.type.Str;
import org.jerlang.type.Term;

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

    public static List getenv() {
        return OSGetenv.getenv_0();
    }

    public static Term getenv(Term varName) {
        return OSGetenv.getenv_1(varName);
    }

    public static Term getenv(Term varName, Term defaultValue) {
        return OSGetenv.getenv_2(varName, defaultValue);
    }

    public static Str getpid() {
        return OSGetpid.getpid_0();
    }

    public static Term putenv(Str varName, Str value) {
        return OSPutenv.putenv_2(varName, value);
    }

}
