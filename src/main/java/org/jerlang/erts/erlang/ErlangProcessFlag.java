package org.jerlang.erts.erlang;

import org.jerlang.erts.init.ProcessFlag;
import org.jerlang.type.Atom;
import org.jerlang.type.Boolean;
import org.jerlang.type.List;
import org.jerlang.type.Term;

public class ErlangProcessFlag {

    private ErlangProcessFlag() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 2:
            Atom flag = params.head().toAtom();
            params = params.tail();
            Term value = params.head();
            return process_flag_2(flag, value);
        default:
            throw Error.badarg;
        }
    }

    /**
     * http://www.erlang.org/doc/man/erlang.html#process_flag-2
     */
    public static Term process_flag_2(Atom flag, Term value) {
        return Boolean.of(ProcessFlag.process_flag(flag, value.isTrue()));
    }

}
