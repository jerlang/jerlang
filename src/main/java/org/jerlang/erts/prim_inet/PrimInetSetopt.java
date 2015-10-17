package org.jerlang.erts.prim_inet;

import static org.jerlang.erts.PrimInet.setopts;

import org.jerlang.erts.erlang.Error;
import org.jerlang.type.List;
import org.jerlang.type.PortID;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;

public class PrimInetSetopt extends AbstractPrimInetFunction {

    private PrimInetSetopt() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 3:
            PortID socket = params.head().toPortID();
            params = params.tail();
            Term opt = params.head();
            params = params.tail();
            Term val = params.head();
            return setopt_3(socket, opt, val);
        default:
            throw new Error("badarg");
        }
    }

    public static Term setopt_3(PortID socket, Term opt, Term val) {
        return setopts(socket, List.of(Tuple.of(opt, val)));
    }

}
