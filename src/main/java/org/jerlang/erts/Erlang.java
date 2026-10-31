package org.jerlang.erts;

import org.jerlang.erts.erlang.ErlangAbs;
import org.jerlang.erts.erlang.ErlangAlias;
import org.jerlang.erts.erlang.ErlangApply;
import org.jerlang.erts.erlang.ErlangAtomEq;
import org.jerlang.erts.erlang.ErlangAtomEqExact;
import org.jerlang.erts.erlang.ErlangAtomGe;
import org.jerlang.erts.erlang.ErlangAtomGt;
import org.jerlang.erts.erlang.ErlangAtomLe;
import org.jerlang.erts.erlang.ErlangAtomLt;
import org.jerlang.erts.erlang.ErlangAtomMinus;
import org.jerlang.erts.erlang.ErlangAtomMultiply;
import org.jerlang.erts.erlang.ErlangAtomNe;
import org.jerlang.erts.erlang.ErlangAtomNeExact;
import org.jerlang.erts.erlang.ErlangAtomPlus;
import org.jerlang.erts.erlang.ErlangAtomPlusPlus;
import org.jerlang.erts.erlang.ErlangBinaryPart;
import org.jerlang.erts.erlang.ErlangBinaryToTerm;
import org.jerlang.erts.erlang.ErlangBitstringToList;
import org.jerlang.erts.erlang.ErlangCancelTimer;
import org.jerlang.erts.erlang.ErlangByteSize;
import org.jerlang.erts.erlang.ErlangDemonitor;
import org.jerlang.erts.erlang.ErlangDiv;
import org.jerlang.erts.erlang.ErlangDisplay;
import org.jerlang.erts.erlang.ErlangElement;
import org.jerlang.erts.erlang.ErlangError;
import org.jerlang.erts.erlang.ErlangExit;
import org.jerlang.erts.erlang.ErlangFunctionExported;
import org.jerlang.erts.erlang.FunApply;
import org.jerlang.erts.erlang.ErlangFunInfoMfa;
import org.jerlang.erts.erlang.ErlangGet;
import org.jerlang.erts.erlang.ErlangMakeFun;
import org.jerlang.erts.erlang.ErlangGetKeys;
import org.jerlang.erts.erlang.ErlangGetModuleInfo;
import org.jerlang.erts.erlang.ErlangHalt;
import org.jerlang.erts.erlang.ErlangHd;
import org.jerlang.erts.erlang.ErlangHibernate;
import org.jerlang.erts.erlang.ErlangIntegerToBinary;
import org.jerlang.erts.erlang.ErlangIntegerToList;
import org.jerlang.erts.erlang.ErlangIsAtom;
import org.jerlang.erts.erlang.ErlangIsBinary;
import org.jerlang.erts.erlang.ErlangIsBitstr;
import org.jerlang.erts.erlang.ErlangIsBoolean;
import org.jerlang.erts.erlang.ErlangIsFunction;
import org.jerlang.erts.erlang.ErlangIsInteger;
import org.jerlang.erts.erlang.ErlangIsAlive;
import org.jerlang.erts.erlang.ErlangIsList;
import org.jerlang.erts.erlang.ErlangIsMap;
import org.jerlang.erts.erlang.ErlangIsMapKey;
import org.jerlang.erts.erlang.ErlangMapGet;
import org.jerlang.erts.erlang.ErlangMapSize;
import org.jerlang.erts.erlang.ErlangMax;
import org.jerlang.erts.erlang.ErlangMin;
import org.jerlang.erts.erlang.ErlangIsNumber;
import org.jerlang.erts.erlang.ErlangIsPid;
import org.jerlang.erts.erlang.ErlangIsProcessAlive;
import org.jerlang.erts.erlang.ErlangIsRecord;
import org.jerlang.erts.erlang.ErlangIsReference;
import org.jerlang.erts.erlang.ErlangIsTuple;
import org.jerlang.erts.erlang.ErlangLength;
import org.jerlang.erts.erlang.ErlangLink;
import org.jerlang.erts.erlang.ErlangListToAtom;
import org.jerlang.erts.erlang.ErlangListToBitstring;
import org.jerlang.erts.erlang.ErlangListToInteger;
import org.jerlang.erts.erlang.ErlangMakeRef;
import org.jerlang.erts.erlang.ErlangMonitor;
import org.jerlang.erts.erlang.ErlangMonitorNode;
import org.jerlang.erts.erlang.ErlangMonotonicTime;
import org.jerlang.erts.erlang.ErlangNode;
import org.jerlang.erts.erlang.ErlangNodes;
import org.jerlang.erts.erlang.ErlangOpenPort;
import org.jerlang.erts.erlang.ErlangPortClose;
import org.jerlang.erts.erlang.ErlangPortControl;
import org.jerlang.erts.erlang.ErlangProcessInfo;
import org.jerlang.erts.erlang.ErlangProcessFlag;
import org.jerlang.erts.erlang.ErlangPut;
import org.jerlang.erts.erlang.ErlangRaise;
import org.jerlang.erts.erlang.ErlangRegister;
import org.jerlang.erts.erlang.ErlangRegistered;
import org.jerlang.erts.erlang.ErlangRem;
import org.jerlang.erts.erlang.ErlangSelf;
import org.jerlang.erts.erlang.ErlangSendAfter;
import org.jerlang.erts.erlang.ErlangSend;
import org.jerlang.erts.erlang.ErlangSetelement;
import org.jerlang.erts.erlang.ErlangSpawn;
import org.jerlang.erts.erlang.ErlangSpawnRequest;
import org.jerlang.erts.erlang.ErlangStartTimer;
import org.jerlang.erts.erlang.ErlangSpawnRequestAbandon;
import org.jerlang.erts.erlang.ErlangSystemInfo;
import org.jerlang.erts.erlang.ErlangTermToBinary;
import org.jerlang.erts.erlang.ErlangThrow;
import org.jerlang.erts.erlang.ErlangTrace;
import org.jerlang.erts.erlang.ErlangTracePattern;
import org.jerlang.erts.erlang.ErlangTl;
import org.jerlang.erts.erlang.ErlangTupleSize;
import org.jerlang.erts.erlang.ErlangTupleToList;
import org.jerlang.erts.erlang.ErlangUnlink;
import org.jerlang.erts.erlang.ErlangUnregister;
import org.jerlang.erts.erlang.ErlangWhereis;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.Atom;
import org.jerlang.type.Binary;
import org.jerlang.type.BitString;
import org.jerlang.type.Fun;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.PID;
import org.jerlang.type.PortID;
import org.jerlang.type.Reference;
import org.jerlang.type.Str;
import org.jerlang.type.Term;
import org.jerlang.type.TimerReference;
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

    public static List atom_plus_plus(List a, List b) {
        return ErlangAtomPlusPlus.atom_plus_plus_2(a, b);
    }

    public static Atom atom_eq(Term a, Term b) {
        return ErlangAtomEq.atom_eq_2(a, b);
    }

    public static Atom atom_eq_exact(Term a, Term b) {
        return ErlangAtomEqExact.atom_eq_exact_2(a, b);
    }

    public static Atom atom_ge(Term a, Term b) {
        return ErlangAtomGe.atom_ge_2(a, b);
    }

    public static Atom atom_gt(Term a, Term b) {
        return ErlangAtomGt.atom_gt_2(a, b);
    }

    public static Atom atom_le(Term a, Term b) {
        return ErlangAtomLe.atom_le_2(a, b);
    }

    public static Atom atom_lt(Term a, Term b) {
        return ErlangAtomLt.atom_lt_2(a, b);
    }

    public static Atom atom_ne(Term a, Term b) {
        return ErlangAtomNe.atom_ne_2(a, b);
    }

    public static Atom atom_ne_exact(Term a, Term b) {
        return ErlangAtomNeExact.atom_ne_exact_2(a, b);
    }

    // --------------------------------------------------------------

    public static Integer abs(Integer integer) {
        return ErlangAbs.abs_1(integer);
    }

    /**
     * erlang:apply/2 applies a fun to an argument list. Erlang has it next to
     * apply/3, and the embedding API needs it to call a fun from Java.
     */
    public static Term apply(Term fun, List args) throws Error {
        return FunApply.apply(fun, args);
    }

    public static Term apply(Term m, Term f, Term a) throws Error {
        return ErlangApply.apply_3(m, f, a);
    }

    public static Binary binary_part(Binary subject, Tuple posLen) {
        return ErlangBinaryPart.binary_part_2(subject, posLen);
    }

    public static Binary binary_part(Binary subject, Integer start, Integer length) {
        return ErlangBinaryPart.binary_part_3(subject, start, length);
    }

    public static Term binary_to_term(Binary binary) {
        return ErlangBinaryToTerm.binary_to_term_1(binary);
    }

    public static List bitstring_to_list(BitString bitString) {
        return ErlangBitstringToList.bitstring_to_list_1(bitString);
    }

    public static Integer byte_size(BitString bitString) {
        return ErlangByteSize.byte_size_1(bitString);
    }

    public static Term display(Term term) {
        return ErlangDisplay.display_1(term);
    }

    public static void error(Term reason) {
        ErlangError.error_1(reason);
    }

    public static void error(Term reason, Term where) {
        ErlangError.error_2(reason, where);
    }

    public static void error(Term reason, Term where, Term options) {
        ErlangError.error_3(reason, where, options);
    }

    public static Term function_exported(Atom module, Atom function, Integer arity) {
        return ErlangFunctionExported.function_exported_3(module, function, arity);
    }

    public static Term fun_info_mfa(Fun fun) {
        return ErlangFunInfoMfa.fun_info_mfa_1(fun);
    }

    public static Fun make_fun(Atom module, Atom function, Integer arity) {
        return ErlangMakeFun.make_fun_3(module, function, arity);
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

    public static Term get_module_info(Atom module, Atom item) {
        return ErlangGetModuleInfo.get_module_info_2(module, item);
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

    public static Term hibernate() {
        return ErlangHibernate.hibernate_0();
    }

    public static Binary integer_to_binary(Integer integer) {
        return ErlangIntegerToBinary.integer_to_binary_1(integer);
    }

    public static Binary integer_to_binary(Integer integer, Integer base) {
        return ErlangIntegerToBinary.integer_to_binary_2(integer, base);
    }

    public static Str integer_to_list(Integer integer) {
        return ErlangIntegerToList.integer_to_list_1(integer);
    }

    public static List integer_to_list(Integer integer, Integer base) {
        return ErlangIntegerToList.integer_to_list_2(integer, base);
    }

    public static Term is_alive() {
        return ErlangIsAlive.is_alive_0();
    }

    public static Term is_atom(Term term) {
        return ErlangIsAtom.is_atom_1(term);
    }

    public static Term is_binary(Term term) {
        return ErlangIsBinary.is_binary_1(term);
    }

    public static Term is_bitstr(Term term) {
        return ErlangIsBitstr.is_bitstr_1(term);
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

    public static Term is_map_key(Term key, org.jerlang.type.Map map) {
        return ErlangIsMapKey.is_map_key_2(key, map);
    }

    public static Term map_get(Term key, org.jerlang.type.Map map) {
        return ErlangMapGet.map_get_2(key, map);
    }

    public static Term min(Term a, Term b) {
        return ErlangMin.min_2(a, b);
    }

    public static Term max(Term a, Term b) {
        return ErlangMax.max_2(a, b);
    }

    public static org.jerlang.type.Integer map_size(org.jerlang.type.Map map) {
        return ErlangMapSize.map_size_1(map);
    }

    public static Term is_number(Term term) {
        return ErlangIsNumber.is_number_1(term);
    }

    public static Term is_pid(Term term) {
        return ErlangIsPid.is_pid_1(term);
    }

    public static Term is_record(Term term, Term recordTag) {
        return ErlangIsRecord.is_record_2(term, recordTag);
    }

    public static Term is_reference(Term term) {
        return ErlangIsReference.is_reference_1(term);
    }

    public static Term is_tuple(Term term) {
        return ErlangIsTuple.is_tuple_1(term);
    }

    public static Integer length(List list) {
        return ErlangLength.length_1(list);
    }

    public static Atom list_to_atom(List string) {
        return ErlangListToAtom.list_to_atom_1(string);
    }

    public static BitString list_to_bitstring(List list) {
        return ErlangListToBitstring.list_to_bitstring_1(list);
    }

    public static Integer list_to_integer(List string) {
        return ErlangListToInteger.list_to_integer_1(string);
    }

    public static Integer list_to_integer(List string, Integer base) {
        return ErlangListToInteger.list_to_integer_2(string, base);
    }

    public static Reference make_ref() {
        return ErlangMakeRef.make_ref_0();
    }

    public static Term raise(Atom exceptionClass, Term reason, Term stacktrace) {
        return ErlangRaise.raise_3(exceptionClass, reason, stacktrace);
    }

    public static Atom node() {
        return ErlangNode.node_0();
    }

    public static Atom node(Term term) {
        return ErlangNode.node_1(term);
    }

    public static List nodes() {
        return ErlangNodes.nodes_0();
    }

    public static List nodes(Term type) {
        return ErlangNodes.nodes_1(type);
    }

    public static PortID open_port(Tuple portName, List portSettings) {
        return ErlangOpenPort.open_port_2(portName, portSettings);
    }

    public static Atom port_close(Term port) {
        return ErlangPortClose.port_close_1(port);
    }

    public static Term port_control(Term portId, Integer operation, Term data) {
        return ErlangPortControl.port_control_3(portId, operation, data);
    }

    public static Term process_flag(Atom flag, Term value) {
        return ErlangProcessFlag.process_flag_2(flag, value);
    }

    public static Term process_info(PID pid) {
        return ErlangProcessInfo.process_info_1(pid);
    }

    public static Term process_info(PID pid, Term item) {
        return ErlangProcessInfo.process_info_2(pid, item);
    }

    public static Term trace(Term target, Term how, List flags) {
        return ErlangTrace.trace_3(target, how, flags);
    }

    public static Term trace_pattern(Term target, Term matchSpec, List flags) {
        return ErlangTracePattern.trace_pattern_3(target, matchSpec, flags);
    }

    public static Term put(Term key, Term val) {
        return ErlangPut.put_2(key, val);
    }

    public static Term element(Integer index, Tuple tuple) {
        return ErlangElement.element_2(index, tuple);
    }

    public static Term register(Atom regName, PID pid) {
        return ErlangRegister.register_2(regName, pid);
    }

    public static Term demonitor(Reference reference) {
        return ErlangDemonitor.demonitor_1(reference);
    }

    public static Term demonitor(Reference reference, List options) {
        return ErlangDemonitor.demonitor_2(reference, options);
    }

    public static Term exit(Term reason) {
        return ErlangExit.exit_1(reason);
    }

    public static Term exit(PID pid, Term reason) {
        return ErlangExit.exit_2(pid, reason);
    }

    public static Term is_process_alive(PID pid) {
        return ErlangIsProcessAlive.is_process_alive_1(pid);
    }

    public static Term link(PID pid) {
        return ErlangLink.link_1(pid);
    }

    public static Term monotonic_time() {
        return ErlangMonotonicTime.monotonic_time_0();
    }

    public static Term monotonic_time(Term unit) {
        return ErlangMonotonicTime.monotonic_time_1(unit);
    }

    public static Term monitor(Atom type, Term target) {
        return ErlangMonitor.monitor_2(type, target);
    }

    public static Term monitor(Atom type, Term target, List options) {
        return ErlangMonitor.monitor_3(type, target, options);
    }

    public static Reference alias() {
        return ErlangAlias.alias_0();
    }

    public static Term unalias(Reference reference) {
        return ErlangAlias.unalias_1(reference);
    }

    public static Term monitor_node(Term node, Term flag) {
        return ErlangMonitorNode.monitor_node_2(node, flag);
    }

    public static Term monitor_node(Term node, Term flag, Term options) {
        return ErlangMonitorNode.monitor_node_3(node, flag, options);
    }

    public static List registered() {
        return ErlangRegistered.registered_0();
    }

    public static Term unlink(PID pid) {
        return ErlangUnlink.unlink_1(pid);
    }

    public static Term unregister(Atom name) {
        return ErlangUnregister.unregister_1(name);
    }

    public static Term whereis(Atom name) {
        return ErlangWhereis.whereis_1(name);
    }

    public static Integer div(Integer a, Integer b) {
        return ErlangDiv.div_2(a, b);
    }

    public static Integer rem(Integer a, Integer b) {
        return ErlangRem.rem_2(a, b);
    }

    public static PID self() {
        return ErlangSelf.self_0();
    }

    public static Term send(Term target, Term message) {
        return ErlangSend.send_2(target, message);
    }

    public static Term send(Term target, Term message, List options) {
        return ErlangSend.send_3(target, message, options);
    }

    public static TimerReference send_after(Integer time, PID pid, Term msg) {
        return ErlangSendAfter.send_after_3(time, pid, msg);
    }

    public static Term cancel_timer(Term timer) {
        return ErlangCancelTimer.cancel_timer_1(timer);
    }

    public static Term cancel_timer(Term timer, List options) {
        return ErlangCancelTimer.cancel_timer_2(timer, options);
    }

    public static Term read_timer(Term timer) {
        return ErlangCancelTimer.read_timer_1(timer);
    }

    public static TimerReference start_timer(Integer time, Term destination, Term message) {
        return ErlangStartTimer.start_timer_3(time, destination, message);
    }

    public static TimerReference start_timer(Integer time, Term destination, Term message,
        Term timeUnit) {
        return ErlangStartTimer.start_timer_4(time, destination, message, timeUnit);
    }

    public static Tuple setelement(Integer index, Tuple tuple, Term value) {
        return ErlangSetelement.setelement_3(index, tuple, value);
    }

    public static PID spawn(Fun fun) {
        return ErlangSpawn.spawn_1(fun);
    }

    public static PID spawn(Atom m, Atom f, List a) {
        return ErlangSpawn.spawn_3(m, f, a);
    }

    public static PID spawn(Atom node, Atom m, Atom f, List a) {
        return ErlangSpawn.spawn_4(node, m, f, a);
    }

    public static PID spawn(Atom node, Fun fun) {
        return ErlangSpawn.spawn_2(node, fun);
    }

    public static Term spawn_link(Fun fun) {
        return ErlangSpawn.spawn_link_1(fun);
    }

    public static Term spawn_link(Atom node, Fun fun) {
        return ErlangSpawn.spawn_link_2(node, fun);
    }

    public static Term spawn_link(Atom m, Atom f, List a) {
        return ErlangSpawn.spawn_link_3(m, f, a);
    }

    public static Term spawn_link(Atom node, Atom m, Atom f, List a) {
        return ErlangSpawn.spawn_link_4(node, m, f, a);
    }

    public static Term spawn_monitor(Fun fun) {
        return ErlangSpawn.spawn_monitor_1(fun);
    }

    public static Term spawn_monitor(Atom m, Atom f, List a) {
        return ErlangSpawn.spawn_monitor_3(m, f, a);
    }

    public static Term spawn_monitor(Atom node, Fun fun) {
        return ErlangSpawn.spawn_monitor_2(node, fun);
    }

    public static Term spawn_opt(Atom node, Fun fun, List options) {
        return ErlangSpawn.spawn_opt_3(node, fun, options);
    }

    public static Term spawn_opt(Fun fun, List options) {
        return ErlangSpawn.spawn_opt_2(fun, options);
    }

    public static Term spawn_opt(Atom m, Atom f, List a, List options) {
        return ErlangSpawn.spawn_opt_4(m, f, a, options);
    }

    public static Term spawn_opt(Atom node, Atom m, Atom f, List a, List options) {
        return ErlangSpawn.spawn_opt_5(node, m, f, a, options);
    }

    public static Reference spawn_request(Atom node, Atom m, Atom f, List a, List options) {
        return ErlangSpawnRequest.spawn_request_5(node, m, f, a, options);
    }

    public static Term spawn_request_abandon(Reference requestId) {
        return ErlangSpawnRequestAbandon.spawn_request_abandon_1(requestId);
    }

    public static Term system_info(Term term) {
        return ErlangSystemInfo.system_info_1(term);
    }

    public static Binary term_to_binary(Term term) {
        return ErlangTermToBinary.term_to_binary_1(term);
    }

    public static void _throw(Term reason) throws ThrowException {
        ErlangThrow.throw_1(reason);
    }

    public static Term tl(List list) {
        return ErlangTl.tl_1(list);
    }

    public static Integer tuple_size(Tuple tuple) {
        return ErlangTupleSize.tuple_size_1(tuple);
    }

    public static List tuple_to_list(Tuple tuple) {
        return ErlangTupleToList.tuple_to_list_1(tuple);
    }

}
