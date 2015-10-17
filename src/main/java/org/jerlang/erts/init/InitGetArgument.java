package org.jerlang.erts.init;

import org.jerlang.erts.Runtime;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Atom;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;

public class InitGetArgument {

    private static final Atom error = Atom.of("error");
    private static final Atom ok = Atom.of("ok");

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 1:
            return get_argument_1(params.head().toAtom());
        default:
            throw Error.badarg;
        }
    }

    /**
     * Returns all values associated with the command line user flag Flag.
     * If Flag is provided several times, each Values is returned in
     * preserved order.
     *
     * There are also a number of flags, which are defined automatically
     * and can be retrieved using this function:
     *
     * `root`::
     * The installation directory of Erlang/OTP, `$ROOT`.
     *
     * `progname`::
     * The name of the program which started Erlang.
     *
     * `home`::
     * The home directory.
     *
     * Returns `error` if there is no value associated with `flag`.
     */
    public static Term get_argument_1(Atom flag) {
        if (Runtime.userFlags().containsKey(flag)) {
            return Tuple.of(ok, Runtime.userFlags().get(flag));
        } else {
            return error;
        }
    }

}
