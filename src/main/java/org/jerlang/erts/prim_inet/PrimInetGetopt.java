package org.jerlang.erts.prim_inet;

import static org.jerlang.erts.PrimInet.getopts;

import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Atom;
import org.jerlang.type.List;
import org.jerlang.type.PortID;
import org.jerlang.type.Term;

public class PrimInetGetopt extends AbstractPrimInetFunction {

    private PrimInetGetopt() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 2:
            PortID socket = params.head().toPortID();
            params = params.tail();
            Atom opt = params.head().toAtom();
            return getopt_2(socket, opt);
        default:
            throw Error.badarg;
        }
    }

    public static Term getopt_2(PortID socket, Atom opt) {
        return getopts(socket, List.of(opt));
    }

}
