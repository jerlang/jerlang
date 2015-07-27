package org.jerlang.erts.erlang;

import org.jerlang.type.List;
import org.jerlang.type.PID;
import org.jerlang.type.Term;

public class ErlangSpawn {

    private ErlangSpawn() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 1:
            return spawn_1(params.head());
        default:
            throw new Error("badarg");
        }
    }

    /**
     * Returns the pid of a new process started by the application of
     * Fun to the empty list []. Otherwise works like spawn/3.
     *
     * http://www.erlang.org/doc/man/erlang.html#spawn-1
     */
    public static PID spawn_1(Term fun) {
        return new PID(0); // TODO
    }

}
