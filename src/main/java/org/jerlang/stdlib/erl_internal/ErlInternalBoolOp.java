package org.jerlang.stdlib.erl_internal;

import org.jerlang.erts.Erlang;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Atom;
import org.jerlang.type.Boolean;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;

public class ErlInternalBoolOp {

    private ErlInternalBoolOp() {
    }

    public static Term dispatch(List params) {
        switch (Erlang.length(params).toInt()) {
        case 2:
            Atom opName = params.head().toAtom();
            params = params.tail();
            Integer arity = params.head().toInteger();
            return bool_op_2(opName, arity);
        default:
            throw new Error("badarg");
        }
    }

    /**
     * Returns `true` if `opName/arity` is a Boolean operator, otherwise `false`.
     *
     * http://www.erlang.org/doc/man/erl_internal.html#bool_op-2
     */
    public static Term bool_op_2(Atom opName, Integer arity) {
        switch (opName.toString() + "/" + arity) {
        case "not/1":
        case "and/2":
        case "or/2":
        case "xor/2":
            return Boolean.am_true;
        default:
            return Boolean.am_false;
        }
    }

}
