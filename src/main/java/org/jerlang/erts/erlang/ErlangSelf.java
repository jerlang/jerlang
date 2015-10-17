package org.jerlang.erts.erlang;

import org.jerlang.ProcessRegistry;
import org.jerlang.type.List;
import org.jerlang.type.PID;
import org.jerlang.type.Term;

public class ErlangSelf {

    private ErlangSelf() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 0:
            return self_0();
        default:
            throw Error.badarg;
        }
    }

    /**
     * http://www.erlang.org/doc/man/erlang.html#self-0
     */
    public static PID self_0() {
        return ProcessRegistry.self().pid();
    }

}
