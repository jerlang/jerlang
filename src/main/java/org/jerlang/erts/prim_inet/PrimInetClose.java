package org.jerlang.erts.prim_inet;

import org.jerlang.erts.erlang.Error;
import org.jerlang.type.List;
import org.jerlang.type.PortID;
import org.jerlang.type.Term;

public class PrimInetClose {

    private PrimInetClose() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 1:
            PortID socket = params.head().toPortID();
            return close_1(socket);
        default:
            throw new Error("badarg");
        }
    }

    public static Term close_1(PortID socket) {
        throw new Error("not implemented");
    }

}
