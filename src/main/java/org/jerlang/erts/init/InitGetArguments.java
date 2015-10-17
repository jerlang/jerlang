package org.jerlang.erts.init;

import org.jerlang.erts.erlang.Error;
import org.jerlang.type.List;
import org.jerlang.type.Term;

public class InitGetArguments {

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 0:
            return get_arguments_0();
        default:
            throw Error.badarg;
        }
    }

    /**
     * Returns all command line flags, as well as the system defined flags,
     * see get_argument/1.
     */
    public static List get_arguments_0() {
        return List.nil; // TODO
    }

}
