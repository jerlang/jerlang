package org.jerlang.erts.erlang;

import org.jerlang.type.Atom;
import org.jerlang.type.Fun;
import org.jerlang.type.List;
import org.jerlang.type.PID;
import org.jerlang.type.Term;
import org.jerlang.vm.VirtualMachine;

public class ErlangSpawn {

    private ErlangSpawn() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 1:
            return spawn_1(params.head().toFun());
        case 3:
            Atom m = params.head().toAtom();
            params = params.tail();
            Atom f = params.head().toAtom();
            params = params.tail();
            List a = params.head().toList();
            return spawn_3(m, f, a);
        default:
            throw Error.badarg;
        }
    }

    /**
     * Returns the pid of a new process started by the application of
     * Fun to the empty list []. Otherwise works like spawn/3.
     *
     * http://www.erlang.org/doc/man/erlang.html#spawn-1
     */
    public static PID spawn_1(Fun fun) {
        return VirtualMachine.instance().spawn(fun).pid();
    }

    public static PID spawn_3(Atom module, Atom fun, List args) {
        return VirtualMachine.instance().spawn(module, fun, args).pid();
    }

}
