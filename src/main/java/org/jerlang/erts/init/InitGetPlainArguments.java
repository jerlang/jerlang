package org.jerlang.erts.init;

import org.jerlang.erts.Runtime;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.List;
import org.jerlang.type.Term;

public class InitGetPlainArguments {

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 0:
            return get_plain_arguments_0();
        default:
            throw new Error("badarg");
        }
    }

    /**
     * Returns any plain command line arguments as a list of strings.
     */
    public static List get_plain_arguments_0() {
        return Runtime.plainArguments();
    }

}
