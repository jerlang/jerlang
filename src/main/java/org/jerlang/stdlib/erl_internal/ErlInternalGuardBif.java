package org.jerlang.stdlib.erl_internal;

import org.jerlang.erts.Erlang;
import org.jerlang.type.Atom;
import org.jerlang.type.Boolean;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;

public class ErlInternalGuardBif {

    private ErlInternalGuardBif() {
    }

    public static Term dispatch(List params) {
        switch (Erlang.length(params).toInt()) {
        case 2:
            Atom name = params.head().toAtom();
            params = params.tail();
            Integer arity = params.head().toInteger();
            return guard_bif_2(name, arity);
        default:
            throw new org.jerlang.erts.erlang.Error("badarg");
        }
    }

    /**
     * Returns `true` if `name/arity` is an Erlang BIF which is allowed in guards,
     * otherwise `false`.
     *
     * http://www.erlang.org/doc/man/erl_internal.html#guard_bif-2
     */
    public static Term guard_bif_2(Atom name, Integer arity) {
        switch (name.toString() + "/" + arity) {
        case "abs/1":
        case "float/1":
        case "trunc/1":
        case "round/1":
        case "length/1":
        case "hd/1":
        case "tl/1":
        case "size/1":
        case "bit_size/1":
        case "byte_size/1":
        case "element/2":
        case "self/0":
        case "map_size/1":
        case "node/0":
        case "node/1":
        case "tuple_size/1":
        case "is_atom/1":
        case "is_binary/1":
        case "is_bitstring/1":
        case "is_boolean/1":
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
        case "binary_part/2":
        case "binary_part/3":
            return Boolean.am_true;
        default:
            return Boolean.am_false;
        }
    }

}
