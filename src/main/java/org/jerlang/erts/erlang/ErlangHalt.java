package org.jerlang.erts.erlang;

import org.jerlang.erts.Erlang;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;

public class ErlangHalt {

    private ErlangHalt() {
    }

    public static Term dispatch(List params) {
        switch (Erlang.length(params).toInt()) {
        case 0:
            halt_0();
            return null;
        case 1:
            halt_1(params.head());
            return null;
        case 2:
            Term status = params.head();
            params = params.tail();
            List options = params.head().toList();
            halt_2(status, options);
            return null;
        default:
            throw new Error("badarg");
        }
    }

    /**
     * The same as `halt(0, [])`.
     */
    public static void halt_0() {
        halt_2(Integer.of(0), List.nil);
    }

    /**
     * The same as halt(Status, []).
     *
     * http://www.erlang.org/doc/man/erlang.html#halt-1
     */
    public static void halt_1(Term status) {
        halt_2(status, List.nil);
    }

    /**
     * Status must be a non-negative integer, a string, or the atom abort.
     * Halts the Erlang runtime system.
     * Has no return value.
     * Depending on Status:
     *
     * integer()::
     * The runtime system exits with the integer value Status as status code
     * to the calling environment (operating system).
     *
     * string()::
     * An erlang crash dump is produced with Status as slogan,
     * and then the runtime system exits with status code 1.
     *
     * abort::
     * The runtime system aborts producing a core dump,
     * if that is enabled in the operating system.
     *
     * Note that on many platforms, only the status codes 0-255 are
     * supported by the operating system.
     *
     * For integer Status the Erlang runtime system closes all ports and
     * allows async threads to finish their operations before exiting.
     * To exit without such flushing use Option as {flush,false}.
     *
     * For statuses string() and abort the flush option is ignored and
     * flushing is not done.
     */
    public static void halt_2(Term status, List options) {
        if (status instanceof Integer) {
            System.exit(((Integer) status).toInt());
        } else {
            System.exit(0);
        }
    }

}
