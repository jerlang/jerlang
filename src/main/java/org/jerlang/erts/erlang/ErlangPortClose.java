package org.jerlang.erts.erlang;

import static org.jerlang.type.Boolean.am_true;

import org.jerlang.Port;
import org.jerlang.ProcessRegistry;
import org.jerlang.ProcessState;
import org.jerlang.type.Atom;
import org.jerlang.type.List;
import org.jerlang.type.PortID;
import org.jerlang.type.Term;

public class ErlangPortClose {

    private ErlangPortClose() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 1:
            return port_close_1(params.head());
        default:
            throw Error.badarg;
        }
    }

    /**
     * Closes an open port.
     * Roughly the same as Port ! {self(), close} except for the error
     * behaviour (see below), being synchronous, and that the port does
     * not reply with {Port, closed}.
     *
     * http://erlang.org/doc/man/erlang.html#port_close-1
     */
    public static Atom port_close_1(Term port) {
        if (port instanceof PortID) {
            Port p = (Port) ProcessRegistry.resolve(port.toPortID());
            p.setState(ProcessState.EXITING);
            return am_true;
        } else {
            throw Error.badarg;
        }
    }

}
