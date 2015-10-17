package org.jerlang.erts.erlang;

import org.jerlang.ProcessRegistry;
import org.jerlang.type.Atom;
import org.jerlang.type.Boolean;
import org.jerlang.type.List;
import org.jerlang.type.PID;
import org.jerlang.type.Term;

public class ErlangRegister {

    private ErlangRegister() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 2:
            Atom regName = params.head().toAtom();
            params = params.tail();
            PID pid = params.head().toPID();
            return register_2(regName, pid);
        default:
            throw Error.badarg;
        }
    }

    /**
     * Associates the name RegName with a pid or a port identifier.
     * RegName, which must be an atom, can be used instead of the
     * pid / port identifier in the send operator (RegName ! Message).
     *
     * http://www.erlang.org/doc/man/erlang.html#register-2
     */
    public static Term register_2(Atom regName, PID pid) {
        ProcessRegistry.register(regName, pid);
        return Boolean.am_true;
    }

}
