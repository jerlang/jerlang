package org.jerlang.erts.init;

import org.jerlang.erts.erlang.Error;
import org.jerlang.type.List;
import org.jerlang.type.Term;

public class InitStop {

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 0:
            stop_0();
            return null;
        default:
            throw Error.badarg;
        }
    }

    public static void stop_0() {
        System.exit(0);
    }

}
