package org.jerlang.stdlib.erl_internal;

import org.jerlang.erts.Erlang;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Atom;
import org.jerlang.type.Boolean;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;

public class ErlInternalArithOp {

    private ErlInternalArithOp() {
    }

    public static Term dispatch(List params) {
        switch (Erlang.length(params).toInt()) {
        case 2:
            Atom opName = params.head().toAtom();
            params = params.tail();
            Integer arity = params.head().toInteger();
            return arith_op_2(opName, arity);
        default:
            throw new Error("badarg");
        }
    }

    /**
     * Returns `true` if `opName/arity` is an arithmetic operator, otherwise `false`.
     *
     * http://www.erlang.org/doc/man/erl_internal.html#arith_op-2
     */
    public static Term arith_op_2(Atom opName, Integer arity) {
        switch (opName.toString() + "/" + arity) {
        case "+/1":
        case "-/1":
        case "*/2":
        case "//2":
        case "+/2":
        case "-/2":
        case "bnot/1":
        case "div/2":
        case "rem/2":
        case "band/2":
        case "bor/2":
        case "bxor/2":
        case "bsl/2":
        case "bsr/2":
            return Boolean.am_true;
        default:
            return Boolean.am_false;
        }
    }

}
