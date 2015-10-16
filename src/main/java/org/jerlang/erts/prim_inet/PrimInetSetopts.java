package org.jerlang.erts.prim_inet;

import org.jerlang.erts.erlang.Error;
import org.jerlang.type.List;
import org.jerlang.type.PortID;
import org.jerlang.type.Term;

public class PrimInetSetopts {

    private PrimInetSetopts() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 2:
            PortID socket = params.head().toPortID();
            params = params.tail();
            List opts = params.head().toList();
            return setopts_2(socket, opts);
        default:
            throw new Error("badarg");
        }
    }

    public static Term setopts_2(PortID socket, List opts) {
        throw new Error("badarg");
    }

}
