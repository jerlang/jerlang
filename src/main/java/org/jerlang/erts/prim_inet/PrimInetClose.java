package org.jerlang.erts.prim_inet;

import static org.jerlang.type.Boolean.am_true;

import org.jerlang.Process;
import org.jerlang.ProcessRegistry;
import org.jerlang.erts.Erlang;
import org.jerlang.erts.PrimInet;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Atom;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.PortID;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;

public class PrimInetClose {

    private static final Atom linger = Atom.of("linger");
    private static final Atom ok = Atom.of("ok");
    private static final Tuple linger_ok = Tuple.of(ok, Tuple.of(am_true, Integer.ZERO));

    private PrimInetClose() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 1:
            PortID socket = params.head().toPortID();
            return close_1(socket);
        default:
            throw Error.badarg;
        }
    }

    public static Term close_1(PortID socket) {
        Term r1 = PrimInet.getopt(socket, linger);
        if (linger_ok.equals(r1)) {
            return close_port(socket);
        } else {
            Process p = ProcessRegistry.self().toProcess();
            // TODO: implement receive to wait until all
            // TODO: messages have been processed
        }
        throw new Error("not implemented");
    }

    private static Term close_port(PortID socket) {
        Erlang.port_close(socket);
        // TODO: catch, receive
        return ok;
    }

}
