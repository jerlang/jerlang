package org.jerlang.stdlib.erl_internal;

import org.jerlang.type.Atom;
import org.jerlang.type.Boolean;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;

public class ErlInternalTypeTest {

    private ErlInternalTypeTest() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 2:
            Atom name = params.head().toAtom();
            params = params.tail();
            Integer arity = params.head().toInteger();
            return type_test_2(name, arity);
        default:
            throw new org.jerlang.erts.erlang.Error("badarg");
        }
    }

    /**
     * Returns `true` if `name/arity` is a valid Erlang type test, otherwise `false`.
     *
     * http://www.erlang.org/doc/man/erl_internal.html#type_test-2
     */
    public static Term type_test_2(Atom name, Integer arity) {
        switch (name.toString() + "/" + arity) {
        case "is_atom/1":
        case "is_boolean/1":
        case "is_binary/1":
        case "is_bitstring/1":
        case "is_float/1":
        case "is_function/1":
        case "is_function/2":
        case "is_integer/1":
        case "is_list/1":
        case "is_map/1":
        case "is_number/1":
        case "is_pid/1":
        case "is_port/1":
        case "is_reference/1":
        case "is_tuple/1":
        case "is_record/2":
        case "is_record/3":
            // Erlang new-style type tests
            return Boolean.am_true;
        case "integer/1":
        case "float/1":
        case "number/1":
        case "atom/1":
        case "list/1":
        case "tuple/1":
        case "pid/1":
        case "reference/1":
        case "port/1":
        case "binary/1":
        case "record/2":
        case "function/1":
            // Erlang old-style type tests
            return Boolean.am_true;
        default:
            return Boolean.am_false;
        }
    }

}
