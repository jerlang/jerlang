package org.jerlang.stdlib;

import org.jerlang.stdlib.erl_internal.ErlInternalArithOp;
import org.jerlang.stdlib.erl_internal.ErlInternalBif;
import org.jerlang.stdlib.erl_internal.ErlInternalBoolOp;
import org.jerlang.stdlib.erl_internal.ErlInternalCompOp;
import org.jerlang.stdlib.erl_internal.ErlInternalGuardBif;
import org.jerlang.stdlib.erl_internal.ErlInternalListOp;
import org.jerlang.stdlib.erl_internal.ErlInternalOpType;
import org.jerlang.stdlib.erl_internal.ErlInternalSendOp;
import org.jerlang.stdlib.erl_internal.ErlInternalTypeTest;
import org.jerlang.type.Atom;
import org.jerlang.type.Integer;
import org.jerlang.type.Term;

/**
 * = erl_internal
 *
 * == MODULE SUMMARY
 *
 * Internal Erlang Definitions
 *
 *
 * == DESCRIPTION
 *
 * This module defines Erlang BIFs, guard tests and operators.
 * This module is only of interest to programmers who manipulate Erlang code.
 *
 * http://www.erlang.org/doc/man/erl_internal.html
 */
public class ErlInternal {

    private ErlInternal() {
    }

    public static Term arith_op(Atom opName, Integer arity) {
        return ErlInternalArithOp.arith_op_2(opName, arity);
    }

    public static Term bif(Atom name, Integer arity) {
        return ErlInternalBif.bif_2(name, arity);
    }

    public static Term bool_op(Atom opName, Integer arity) {
        return ErlInternalBoolOp.bool_op_2(opName, arity);
    }

    public static Term comp_op(Atom opName, Integer arity) {
        return ErlInternalCompOp.comp_op_2(opName, arity);
    }

    public static Term guard_bif(Atom name, Integer arity) {
        return ErlInternalGuardBif.guard_bif_2(name, arity);
    }

    public static Term list_op(Atom opName, Integer arity) {
        return ErlInternalListOp.list_op_2(opName, arity);
    }

    public static Atom op_type(Atom opName, Integer arity) {
        return ErlInternalOpType.op_type_2(opName, arity);
    }

    public static Term send_op(Atom opName, Integer arity) {
        return ErlInternalSendOp.send_op_2(opName, arity);
    }

    public static Term type_test(Atom name, Integer arity) {
        return ErlInternalTypeTest.type_test_2(name, arity);
    }

}
