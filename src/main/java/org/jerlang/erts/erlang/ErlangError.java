package org.jerlang.erts.erlang;

import org.jerlang.erts.Erlang;
import org.jerlang.type.List;
import org.jerlang.type.Term;

public class ErlangError {

    private ErlangError() {
    }

    public static Term dispatch(List params) {
        switch (Erlang.length(params).toInt()) {
        case 1:
            error_1(params.head());
            return null;
        case 2:
            Term reason = params.head();
            params = params.tail();
            List where = params.head().toList();
            error_2(reason, where);
            return null;
        default:
            throw new Error("badarg");
        }
    }

    /**
     * Stops the execution of the calling process with the reason `reason`,
     * where `reason` is any term. The actual exit reason will be
     * `{reason, where}`, where `where` is a list of the functions most
     * recently called (the current function first). Since evaluating this
     * function causes the process to terminate, it has no return value.
     *
     * http://www.erlang.org/doc/man/erlang.html#error-1
     */
    public static void error_1(Term reason) {
        throw new Error(reason);
    }

    /**
     * Stops the execution of the calling process with the reason `reason`,
     * where `reason` is any term. The actual exit reason will be
     * `{reason, where}`, where `where` is a list of the functions most
     * recently called (the current function first). `args` is expected to
     * be the list of arguments for the current function; in Beam it will be
     * used to provide the actual arguments for the current function in the
     * `where` term. Since evaluating this function causes the process to
     * terminate, it has no return value.
     *
     * http://www.erlang.org/doc/man/erlang.html#error-2
     */
    public static void error_2(Term reason, List args) {
        throw new Error(reason, args);
    }

}
