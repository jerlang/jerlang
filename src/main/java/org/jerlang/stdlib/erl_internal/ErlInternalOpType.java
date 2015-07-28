package org.jerlang.stdlib.erl_internal;

import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Atom;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;

public class ErlInternalOpType {

    private static final Atom arith = Atom.of("arith");
    private static final Atom bool = Atom.of("bool");
    private static final Atom comp = Atom.of("comp");
    private static final Atom list = Atom.of("list");
    private static final Atom send = Atom.of("send");

    private ErlInternalOpType() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 2:
            Atom opName = params.head().toAtom();
            params = params.tail();
            Integer arity = params.head().toInteger();
            return op_type_2(opName, arity);
        default:
            throw new org.jerlang.erts.erlang.Error("badarg");
        }
    }

    /**
     * Returns the `Type` of operator that `opName/arity` belongs to,
     * or generates a `function_clause` error if it is not an operator at all.
     *
     * http://www.erlang.org/doc/man/erl_internal.html#op_type-2
     */
    public static Atom op_type_2(Atom opName, Integer arity) {
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
            return arith;
        case "not/1":
        case "and/2":
        case "or/2":
        case "xor/2":
            return bool;
        case "==/2":
        case "/=/2":
        case "=</2":
        case "</2":
        case ">=/2":
        case ">/2":
        case "=:=/2":
        case "=/=/2":
            return comp;
        case "++/2":
        case "--/2":
            return list;
        case "!/2":
            return send;
        default:
            throw new Error("no function clause matching ErlInternal.op_type(" + opName + ","
                + arity + ")");
        }
    }

}
