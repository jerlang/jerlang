package org.jerlang.kernel.net_adm;

import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Atom;
import org.jerlang.type.List;
import org.jerlang.type.Term;

public class NetAdmPing {

    private static final Atom pang = Atom.of("pang");
    private static final Atom pong = Atom.of("pong");

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 1:
            return ping_1(params.head().toAtom());
        default:
            throw Error.badarg;
        }
    }

    /**
     * Tries to set up a connection to Node.
     * Returns pang if it fails, or pong if it is successful.
     *
     * http://www.erlang.org/doc/man/net_adm.html#ping-1
     */
    public static Atom ping_1(Atom node) {
        return pang;
    }

}
