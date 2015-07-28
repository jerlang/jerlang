package org.jerlang.stdlib.erl_internal;

import org.jerlang.type.Atom;
import org.jerlang.type.Boolean;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;

public class ErlInternalSendOp {

    private ErlInternalSendOp() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 2:
            Atom opName = params.head().toAtom();
            params = params.tail();
            Integer arity = params.head().toInteger();
            return send_op_2(opName, arity);
        default:
            throw new org.jerlang.erts.erlang.Error("badarg");
        }
    }

    /**
     * Returns `true` if `opName/arity` is a send operator, otherwise `false`.
     *
     * http://www.erlang.org/doc/man/erl_internal.html#send_op-2
     */
    public static Term send_op_2(Atom opName, Integer arity) {
        switch (opName.toString() + "/" + arity) {
        case "!/2":
            return Boolean.am_true;
        default:
            return Boolean.am_false;
        }
    }

}
