package org.jerlang.stdlib;

import org.jerlang.erts.Erlang;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Atom;
import org.jerlang.type.Boolean;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
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
 */
public class ErlInternal {

    public static final String[] EXPORT = {
        "arith_op/2",
        "bif/2",
        "bool_op/2",
        "comp_op/2",
        "guard_bif/2",
        "list_op/2",
        "op_type/2",
        "send_op/2",
        "type_test/2"
    };

    private static final Atom arith = Atom.of("arith");
    private static final Atom bool = Atom.of("bool");
    private static final Atom comp = Atom.of("comp");
    private static final Atom list = Atom.of("list");
    private static final Atom send = Atom.of("send");

    private ErlInternal() {
    }

    /**
     * Returns `true` if `opName/arity` is an arithmetic operator, otherwise `false`.
     */
    public static Term arith_op(List params) {
        switch (Erlang.length_1(params).toInt()) {
        case 2:
            Atom opName = params.head().toAtom();
            params = params.tail();
            Integer arity = params.head().toInteger();
            return Boolean.of(arith_op_2(opName, arity));
        default:
            throw new Error("badarg");
        }
    }

    public static boolean arith_op_2(Atom opName, Integer arity) {
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
            return true;
        default:
            return false;
        }
    }

    /**
     * Returns `true` if `name/arity` is an Erlang BIF which is automatically
     * recognized by the compiler, otherwise `false`.
     */
    public static Term bif(List params) {
        switch (Erlang.length_1(params).toInt()) {
        case 2:
            Atom name = params.head().toAtom();
            params = params.tail();
            Integer arity = params.head().toInteger();
            return Boolean.of(bif_2(name, arity));
        default:
            throw new Error("badarg");
        }
    }

    public static boolean bif_2(Atom name, Integer arity) {
        switch (name.toString() + "/" + arity) {
        case "abs/1":
        case "apply/2":
        case "apply/3":
        case "atom_to_binary/2":
        case "atom_to_list/1":
        case "binary_part/2":
        case "binary_part/3":
        case "binary_to_atom/2":
        case "binary_to_existing_atom/2":
        case "binary_to_integer/1":
        case "binary_to_integer/2":
        case "binary_to_float/1":
        case "binary_to_list/1":
        case "binary_to_list/3":
        case "binary_to_term/1":
        case "binary_to_term/2":
        case "bitsize/1":
        case "bit_size/1":
        case "bitstring_to_list/1":
        case "byte_size/1":
        case "check_old_code/1":
        case "check_process_code/2":
        case "check_process_code/3":
        case "date/0":
        case "delete_module/1":
        case "demonitor/1":
        case "demonitor/2":
        case "disconnect_node/1":
        case "element/2":
        case "erase/0":
        case "erase/1":
        case "error/1":
        case "error/2":
        case "exit/1":
        case "exit/2":
        case "float/1":
        case "float_to_list/1":
        case "float_to_list/2":
        case "float_to_binary/1":
        case "float_to_binary/2":
        case "garbage_collect/0":
        case "garbage_collect/1":
        case "garbage_collect/2":
        case "get/0":
        case "get/1":
        case "get_keys/0":
        case "get_keys/1":
        case "group_leader/0":
        case "group_leader/2":
        case "halt/0":
        case "halt/1":
        case "halt/2":
        case "hd/1":
        case "integer_to_binary/1":
        case "integer_to_binary/2":
        case "integer_to_list/1":
        case "integer_to_list/2":
        case "iolist_size/1":
        case "iolist_to_binary/1":
        case "is_alive/0":
        case "is_process_alive/1":
        case "is_atom/1":
        case "is_boolean/1":
        case "is_binary/1":
        case "is_bitstr/1":
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
        case "length/1":
        case "link/1":
        case "list_to_atom/1":
        case "list_to_binary/1":
        case "list_to_bitstring/1":
        case "list_to_existing_atom/1":
        case "list_to_float/1":
        case "list_to_integer/1":
        case "list_to_integer/2":
        case "list_to_pid/1":
        case "list_to_tuple/1":
        case "load_module/2":
        case "make_ref/0":
        case "map_size,1":
        case "max,2":
        case "min,2":
        case "module_loaded/1":
        case "monitor/2":
        case "monitor/3":
        case "monitor_node/2":
        case "node/0":
        case "node/1":
        case "nodes/0":
        case "nodes/1":
        case "now/0":
        case "open_port/2":
        case "pid_to_list/1":
        case "port_close/1":
        case "port_command/2":
        case "port_command/3":
        case "port_connect/2":
        case "port_control/3":
        case "pre_loaded/0":
        case "process_flag/2":
        case "process_flag/3":
        case "process_info/1":
        case "process_info/2":
        case "processes/0":
        case "purge_module/1":
        case "put/2":
        case "register/2":
        case "registered/0":
        case "round/1":
        case "self/0":
        case "setelement/3":
        case "size/1":
        case "spawn/1":
        case "spawn/2":
        case "spawn/3":
        case "spawn/4":
        case "spawn_link/1":
        case "spawn_link/2":
        case "spawn_link/3":
        case "spawn_link/4":
        case "spawn_monitor/1":
        case "spawn_monitor/3":
        case "spawn_opt/2":
        case "spawn_opt/3":
        case "spawn_opt/4":
        case "spawn_opt/5":
        case "split_binary/2":
        case "statistics/1":
        case "term_to_binary/1":
        case "term_to_binary/2":
        case "throw/1":
        case "time/0":
        case "tl/1":
        case "trunc/1":
        case "tuple_size/1":
        case "tuple_to_list/1":
        case "unlink/1":
        case "unregister/1":
        case "whereis/1":
            return true;
        default:
            return false;
        }
    }

    /**
     * Returns `true` if `name/arity` is an Erlang BIF which is allowed in guards,
     * otherwise `false`.
     */
    public static Term guard_bif(List params) {
        switch (Erlang.length_1(params).toInt()) {
        case 2:
            Atom name = params.head().toAtom();
            params = params.tail();
            Integer arity = params.head().toInteger();
            return Boolean.of(guard_bif_2(name, arity));
        default:
            throw new org.jerlang.erts.erlang.Error("badarg");
        }
    }

    public static boolean guard_bif_2(Atom name, Integer arity) {
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
            return true;
        default:
            return false;
        }
    }

    /**
     * Returns `true` if `name/arity` is a valid Erlang type test, otherwise `false`.
     */
    public static Term type_test(List params) {
        switch (Erlang.length_1(params).toInt()) {
        case 2:
            Atom name = params.head().toAtom();
            params = params.tail();
            Integer arity = params.head().toInteger();
            return Boolean.of(type_test_2(name, arity));
        default:
            throw new org.jerlang.erts.erlang.Error("badarg");
        }
    }

    public static boolean type_test_2(Atom name, Integer arity) {
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
            return true;
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
            return true;
        default:
            return false;
        }
    }

    /**
     * Returns `true` if `opName/arity` is a Boolean operator, otherwise `false`.
     */
    public static Term bool_op(List params) {
        switch (Erlang.length_1(params).toInt()) {
        case 2:
            Atom opName = params.head().toAtom();
            params = params.tail();
            Integer arity = params.head().toInteger();
            return Boolean.of(bool_op_2(opName, arity));
        default:
            throw new Error("badarg");
        }
    }

    public static boolean bool_op_2(Atom opName, Integer arity) {
        switch (opName.toString() + "/" + arity) {
        case "not/1":
        case "and/2":
        case "or/2":
        case "xor/2":
            return true;
        default:
            return false;
        }
    }

    /**
     * Returns `true` if `opName/arity` is a comparison operator, otherwise `false`.
     */
    public static Term comp_op(List params) {
        switch (Erlang.length_1(params).toInt()) {
        case 2:
            Atom opName = params.head().toAtom();
            params = params.tail();
            Integer arity = params.head().toInteger();
            return Boolean.of(comp_op_2(opName, arity));
        default:
            throw new Error("badarg");
        }
    }

    public static boolean comp_op_2(Atom opName, Integer arity) {
        switch (opName.toString() + "/" + arity) {
        case "==/2":
        case "/=/2":
        case "=</2":
        case "</2":
        case ">=/2":
        case ">/2":
        case "=:=/2":
        case "=/=/2":
            return true;
        default:
            return false;
        }
    }

    /**
     * Returns `true` if `opName/arity` is a list operator, otherwise `false`.
     */
    public static Term list_op(List params) {
        switch (Erlang.length_1(params).toInt()) {
        case 2:
            Atom opName = params.head().toAtom();
            params = params.tail();
            Integer arity = params.head().toInteger();
            return Boolean.of(list_op_2(opName, arity));
        default:
            throw new org.jerlang.erts.erlang.Error("badarg");
        }
    }

    public static boolean list_op_2(Atom opName, Integer arity) {
        switch (opName.toString() + "/" + arity) {
        case "++/2":
        case "--/2":
            return true;
        default:
            return false;
        }
    }

    /**
     * Returns `true` if `opName/arity` is a send operator, otherwise `false`.
     */
    public static Term send_op(List params) {
        switch (Erlang.length_1(params).toInt()) {
        case 2:
            Atom opName = params.head().toAtom();
            params = params.tail();
            Integer arity = params.head().toInteger();
            return Boolean.of(send_op_2(opName, arity));
        default:
            throw new org.jerlang.erts.erlang.Error("badarg");
        }
    }

    public static boolean send_op_2(Atom opName, Integer arity) {
        switch (opName.toString() + "/" + arity) {
        case "!/2":
            return true;
        default:
            return false;
        }
    }

    /**
     * Returns the `Type` of operator that `opName/arity` belongs to,
     * or generates a `function_clause` error if it is not an operator at all.
     */
    public static Term op_type(List params) {
        switch (Erlang.length_1(params).toInt()) {
        case 2:
            Atom opName = params.head().toAtom();
            params = params.tail();
            Integer arity = params.head().toInteger();
            return op_type_2(opName, arity);
        default:
            throw new org.jerlang.erts.erlang.Error("badarg");
        }
    }

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
