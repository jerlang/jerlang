package org.jerlang.erts;

import org.jerlang.erts.erlang.ErlangAbs;
import org.jerlang.erts.erlang.ErlangApply;
import org.jerlang.erts.erlang.ErlangAtomMinus;
import org.jerlang.erts.erlang.ErlangAtomMultiply;
import org.jerlang.erts.erlang.ErlangAtomPlus;
import org.jerlang.erts.erlang.ErlangDisplay;
import org.jerlang.erts.erlang.ErlangError;
import org.jerlang.erts.erlang.ErlangFunctionExported;
import org.jerlang.erts.erlang.ErlangGet;
import org.jerlang.erts.erlang.ErlangGetKeys;
import org.jerlang.erts.erlang.ErlangHalt;
import org.jerlang.erts.erlang.ErlangHd;
import org.jerlang.erts.erlang.ErlangIntegerToBinary;
import org.jerlang.erts.erlang.ErlangIntegerToList;
import org.jerlang.erts.erlang.ErlangIsAtom;
import org.jerlang.erts.erlang.ErlangIsBinary;
import org.jerlang.erts.erlang.ErlangIsBoolean;
import org.jerlang.erts.erlang.ErlangIsFunction;
import org.jerlang.erts.erlang.ErlangIsInteger;
import org.jerlang.erts.erlang.ErlangIsList;
import org.jerlang.erts.erlang.ErlangIsMap;
import org.jerlang.erts.erlang.ErlangIsNumber;
import org.jerlang.erts.erlang.ErlangIsPid;
import org.jerlang.erts.erlang.ErlangIsTuple;
import org.jerlang.erts.erlang.ErlangLength;
import org.jerlang.erts.erlang.ErlangProcessFlag;
import org.jerlang.erts.erlang.ErlangRegister;
import org.jerlang.erts.erlang.ErlangRem;
import org.jerlang.erts.erlang.ErlangSelf;
import org.jerlang.erts.erlang.ErlangSpawn;
import org.jerlang.erts.erlang.ErlangTupleSize;
import org.jerlang.erts.erlang.ErlangTupleToList;
import org.jerlang.type.Atom;
import org.jerlang.type.Binary;
import org.jerlang.type.Fun;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.PID;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;

/**
 * http://www.erlang.org/doc/man/erlang.html
 */
public class Erlang {

    public static Integer atom_minus(Integer a, Integer b) {
        return ErlangAtomMinus.atom_minus_2(a, b);
    }

    public static Integer atom_multiply(Integer a, Integer b) {
        return ErlangAtomMultiply.atom_multiply_2(a, b);
    }

    public static Integer atom_plus(Integer a, Integer b) {
        return ErlangAtomPlus.atom_plus_2(a, b);
    }

    // --------------------------------------------------------------

    public static Integer abs(Integer integer) {
        return ErlangAbs.abs_1(integer);
    }

    public static Term apply(Term m, Term f, Term a) {
        return ErlangApply.apply_3(m, f, a);
    }

    public static Term display(Term term) {
        return ErlangDisplay.display_1(term);
    }

    public static void error(Term reason) {
        ErlangError.error_1(reason);
    }

    public static void error(Term reason, List where) {
        ErlangError.error_2(reason, where);
    }

    public static Term function_exported(Atom module, Atom function, Integer arity) {
        return ErlangFunctionExported.function_exported_3(module, function, arity);
    }

    public static List get() {
        return ErlangGet.get_0();
    }

    public static Term get(Term key) {
        return ErlangGet.get_1(key);
    }

    public static List get_keys() {
        return ErlangGetKeys.get_keys_0();
    }

    public static List get_keys(Term value) {
        return ErlangGetKeys.get_keys_1(value);
    }

    public static void halt() {
        ErlangHalt.halt_0();
    }

    public static void halt(Term status) {
        ErlangHalt.halt_1(status);
    }

    public static void halt(Term status, List options) {
        ErlangHalt.halt_2(status, options);
    }

    public static Term hd(List list) {
        return ErlangHd.hd_1(list);
    }

    public static Binary integer_to_binary(Integer integer) {
        return ErlangIntegerToBinary.integer_to_binary_1(integer);
    }

    public static Binary integer_to_binary(Integer integer, Integer base) {
        return ErlangIntegerToBinary.integer_to_binary_2(integer, base);
    }

    public static List integer_to_list(Integer integer) {
        return ErlangIntegerToList.integer_to_list_1(integer);
    }

    public static List integer_to_list(Integer integer, Integer base) {
        return ErlangIntegerToList.integer_to_list_2(integer, base);
    }

    public static Term is_atom(Term term) {
        return ErlangIsAtom.is_atom_1(term);
    }

    public static Term is_binary(Term term) {
        return ErlangIsBinary.is_binary_1(term);
    }

    public static Term is_boolean(Term term) {
        return ErlangIsBoolean.is_boolean_1(term);
    }

    public static Term is_function(Term term) {
        return ErlangIsFunction.is_function_1(term);
    }

    public static Term is_integer(Term term) {
        return ErlangIsInteger.is_integer_1(term);
    }

    public static Term is_list(Term term) {
        return ErlangIsList.is_list_1(term);
    }

    public static Term is_map(Term term) {
        return ErlangIsMap.is_map_1(term);
    }

    public static Term is_number(Term term) {
        return ErlangIsNumber.is_number_1(term);
    }

    public static Term is_pid(Term term) {
        return ErlangIsPid.is_pid_1(term);
    }

    public static Term is_tuple(Term term) {
        return ErlangIsTuple.is_tuple_1(term);
    }

    public static Integer length(List list) {
        return ErlangLength.length_1(list);
    }

    public static Term process_flag(Atom flag, Term value) {
        return ErlangProcessFlag.process_flag_2(flag, value);
    }

    public static Term register(Atom regName, PID pid) {
        return ErlangRegister.register_2(regName, pid);
    }

    public static Integer rem(Integer a, Integer b) {
        return ErlangRem.rem_2(a, b);
    }

    public static PID self() {
        return ErlangSelf.self_0();
    }

    public static PID spawn(Fun fun) {
        return ErlangSpawn.spawn_1(fun);
    }

    public static Integer tuple_size(Tuple tuple) {
        return ErlangTupleSize.tuple_size_1(tuple);
    }

    public static List tuple_to_list(Tuple tuple) {
        return ErlangTupleToList.tuple_to_list_1(tuple);
    }

}
