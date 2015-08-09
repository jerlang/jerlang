package org.jerlang;

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;

import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.jerlang.util.StringUtil;

/**
 * Based on:
 * https://github.com/erlang/otp/blob/master/lib/compiler/src/genop.tab
 */
public enum Opcode {

    /** Implemented in org.jerlang.erts.emulator.op.Label */
    label(1, 1),

    /** Implemented in org.jerlang.erts.emulator.op.FuncInfo */
    func_info(2, 3),

    /** Implemented in org.jerlang.erts.emulator.op.IntCodeEnd */
    int_code_end(3, 0),

    // ------------------------------------------------------------------------
    // Function and BIF calls.

    /** Implemented in org.jerlang.erts.emulator.op.Call */
    call(4, 2),

    /** Implemented in org.jerlang.erts.emulator.op.CallLast */
    call_last(5, 3),

    /** Implemented in org.jerlang.erts.emulator.op.CallOnly */
    call_only(6, 2),

    /** Implemented in org.jerlang.erts.emulator.op.CallExt */
    call_ext(7, 2),

    /** Implemented in org.jerlang.erts.emulator.op.CallExtLast */
    call_ext_last(8, 3),

    /** Implemented in org.jerlang.erts.emulator.op.Bif0 */
    bif0(9, 2),

    /** Implemented in org.jerlang.erts.emulator.op.Bif1 */
    bif1(10, 4),

    /** Implemented in org.jerlang.erts.emulator.op.Bif2 */
    bif2(11, 5),

    // ------------------------------------------------------------------------
    // Allocating, deallocating and returning.

    /** Implemented in org.jerlang.erts.emulator.op.Allocate */
    allocate(12, 2),

    /** Implemented in org.jerlang.erts.emulator.op.AllocateHeap */
    allocate_heap(13, 3),

    /** Implemented in org.jerlang.erts.emulator.op.AllocateZero */
    allocate_zero(14, 2),

    /** Implemented in org.jerlang.erts.emulator.op.AllocateHeapZero */
    allocate_heap_zero(15, 3),

    /** Implemented in org.jerlang.erts.emulator.op.TestHeap */
    test_heap(16, 2),

    /** Implemented in org.jerlang.erts.emulator.op.Init */
    init(17, 1),

    /** Implemented in org.jerlang.erts.emulator.op.Deallocate */
    deallocate(18, 1),

    /** Implemented in org.jerlang.erts.emulator.op.Return */
    _return(19, 0),

    // ------------------------------------------------------------------------
    // Sending & receiving.

    /** Implemented in org.jerlang.erts.emulator.op.Send */
    send(20, 0),

    /** Implemented in org.jerlang.erts.emulator.op.RemoveMessage */
    remove_message(21, 0),

    /** Implemented in org.jerlang.erts.emulator.op.Timeout */
    timeout(22, 0),

    /** Implemented in org.jerlang.erts.emulator.op.LoopRec */
    loop_rec(23, 2),

    /** Implemented in org.jerlang.erts.emulator.op.LoopRecEnd */
    loop_rec_end(24, 1),

    /** Implemented in org.jerlang.erts.emulator.op.Wait */
    wait(25, 1),

    /** Implemented in org.jerlang.erts.emulator.op.WaitTimeout */
    wait_timeout(26, 2),

    // ------------------------------------------------------------------------
    // Arithmetic opcodes.

    deprecated_m_plus(27, 4),
    deprecated_m_minus(28, 4),
    deprecated_m_times(29, 4),
    deprecated_m_div(30, 4),
    deprecated_int_div(31, 4),
    deprecated_int_rem(32, 4),
    deprecated_int_band(33, 4),
    deprecated_int_bor(34, 4),
    deprecated_int_bxor(35, 4),
    deprecated_int_bsl(36, 4),
    deprecated_int_bsr(37, 4),
    deprecated_int_bnot(38, 3),

    // ------------------------------------------------------------------------
    // Comparision operators.

    /** Implemented in org.jerlang.erts.emulator.op.IsLt */
    is_lt(39, 3),

    /** Implemented in org.jerlang.erts.emulator.op.IsGe */
    is_ge(40, 3),

    /** Implemented in org.jerlang.erts.emulator.op.IsEq */
    is_eq(41, 3),

    /** Implemented in org.jerlang.erts.emulator.op.IsNe */
    is_ne(42, 3),

    /** Implemented in org.jerlang.erts.emulator.op.IsEqExact */
    is_eq_exact(43, 3),

    /** Implemented in org.jerlang.erts.emulator.op.IsNeExact */
    is_ne_exact(44, 3),

    // ------------------------------------------------------------------------
    // Type tests.

    /** Implemented in org.jerlang.erts.emulator.op.IsInteger */
    is_integer(45, 2),

    /** Implemented in org.jerlang.erts.emulator.op.IsFloat */
    is_float(46, 2),

    /** Implemented in org.jerlang.erts.emulator.op.IsNumber */
    is_number(47, 2),

    /** Implemented in org.jerlang.erts.emulator.op.IsAtom */
    is_atom(48, 2),

    /** Implemented in org.jerlang.erts.emulator.op.IsPid */
    is_pid(49, 2),

    /** Implemented in org.jerlang.erts.emulator.op.IsReference */
    is_reference(50, 2),

    /** Implemented in org.jerlang.erts.emulator.op.IsPort */
    is_port(51, 2),

    /** Implemented in org.jerlang.erts.emulator.op.IsNil */
    is_nil(52, 2),

    /** Implemented in org.jerlang.erts.emulator.op.IsBinary */
    is_binary(53, 2),

    deprecated_is_constant(54, 2),

    /** Implemented in org.jerlang.erts.emulator.op.IsList */
    is_list(55, 2),

    /** Implemented in org.jerlang.erts.emulator.op.IsNonemptyList */
    is_nonempty_list(56, 2),

    /** Implemented in org.jerlang.erts.emulator.op.IsTuple */
    is_tuple(57, 2),

    /** Implemented in org.jerlang.erts.emulator.op.TestArity */
    test_arity(58, 3),

    // ------------------------------------------------------------------------
    // Indexing & jumping.

    /** Implemented in org.jerlang.erts.emulator.op.SelectVal */
    select_val(59, 3),

    /** Implemented in org.jerlang.erts.emulator.op.SelectTupleArity */
    select_tuple_arity(60, 3),

    /** Implemented in org.jerlang.erts.emulator.op.Jump */
    jump(61, 1),

    // ------------------------------------------------------------------------
    // Catch.

    /** Implemented in org.jerlang.erts.emulator.op.Catch */
    _catch(62, 2),

    /** Implemented in org.jerlang.erts.emulator.op.CatchEnd */
    catch_end(63, 1),

    // ------------------------------------------------------------------------
    // Moving, extracting, modifying.

    /** Implemented in org.jerlang.erts.emulator.op.Move */
    move(64, 2),

    /** Implemented in org.jerlang.erts.emulator.op.GetList */
    get_list(65, 3),

    /** Implemented in org.jerlang.erts.emulator.op.GetTupleElement */
    get_tuple_element(66, 3),

    /** Implemented in org.jerlang.erts.emulator.op.SetTupleElement */
    set_tuple_element(67, 3),

    // ------------------------------------------------------------------------
    // Building terms.

    deprecated_put_string(68, 3),

    /** Implemented in org.jerlang.erts.emulator.op.PutList */
    put_list(69, 3),

    /** Implemented in org.jerlang.erts.emulator.op.PutTuple */
    put_tuple(70, 2),

    /** Implemented in org.jerlang.erts.emulator.op.Put */
    put(71, 1),

    // ------------------------------------------------------------------------
    // Raising errors.

    /** Implemented in org.jerlang.erts.emulator.op.Badmatch */
    badmatch(72, 1),

    /** Implemented in org.jerlang.erts.emulator.op.IfEnd */
    if_end(73, 0),

    /** Implemented in org.jerlang.erts.emulator.op.CaseEnd */
    case_end(74, 1),

    // ------------------------------------------------------------------------
    // 'fun' support.

    /** Implemented in org.jerlang.erts.emulator.op.CallFun */
    call_fun(75, 1),

    deprecated_make_fun(76, 3),

    /** Implemented in org.jerlang.erts.emulator.op.IsFunction */
    is_function(77, 2),

    // ------------------------------------------------------------------------
    // Late additions to R5.

    /** Implemented in org.jerlang.erts.emulator.op.CallExtOnly */
    call_ext_only(78, 2),

    // ------------------------------------------------------------------------
    // Binary matching (R7).

    deprecated_bs_start_match(79, 2),
    deprecated_bs_get_integer(80, 5),
    deprecated_bs_get_float(81, 5),
    deprecated_bs_get_binary(82, 5),
    deprecated_bs_skip_bits(83, 4),
    deprecated_bs_test_tail(84, 2),
    deprecated_bs_save(85, 1),
    deprecated_bs_restore(86, 1),

    // ------------------------------------------------------------------------
    // Binary construction (R7A).

    deprecated_bs_init(87, 2),
    deprecated_bs_final(88, 2),

    /** Implemented in org.jerlang.erts.emulator.op.BsPutInteger */
    bs_put_integer(89, 5),

    /** Implemented in org.jerlang.erts.emulator.op.BsPutBinary */
    bs_put_binary(90, 5),

    /** Implemented in org.jerlang.erts.emulator.op.BsPutFloat */
    bs_put_float(91, 5),

    /** Implemented in org.jerlang.erts.emulator.op.BsPutString */
    bs_put_string(92, 2),

    // ------------------------------------------------------------------------
    // Binary construction (R7B).

    deprecated_bs_need_buf(93, 1),

    // ------------------------------------------------------------------------
    // Floating point arithmetic (R8).

    /** Implemented in org.jerlang.erts.emulator.op.Fclearerror */
    fclearerror(94, 0),

    /** Implemented in org.jerlang.erts.emulator.op.Fcheckerror */
    fcheckerror(95, 1),

    /** Implemented in org.jerlang.erts.emulator.op.Fmove */
    fmove(96, 2),

    /** Implemented in org.jerlang.erts.emulator.op.Fconv */
    fconv(97, 2),

    /** Implemented in org.jerlang.erts.emulator.op.Fadd */
    fadd(98, 4),

    /** Implemented in org.jerlang.erts.emulator.op.Fsub */
    fsub(99, 4),

    /** Implemented in org.jerlang.erts.emulator.op.Fmul */
    fmul(100, 4),

    /** Implemented in org.jerlang.erts.emulator.op.Fdiv */
    fdiv(101, 4),

    /** Implemented in org.jerlang.erts.emulator.op.Fnegate */
    fnegate(102, 3),

    // ------------------------------------------------------------------------
    // New fun construction (R8).

    /** Implemented in org.jerlang.erts.emulator.op.MakeFun2 */
    make_fun2(103, 1),

    // ------------------------------------------------------------------------
    // Try/catch/raise (R10B).

    /** Implemented in org.jerlang.erts.emulator.op.Try */
    _try(104, 2),

    /** Implemented in org.jerlang.erts.emulator.op.TryEnd */
    try_end(105, 1),

    /** Implemented in org.jerlang.erts.emulator.op.TryCase */
    try_case(106, 1),

    /** Implemented in org.jerlang.erts.emulator.op.TryCaseEnd */
    try_case_end(107, 1),

    /** Implemented in org.jerlang.erts.emulator.op.Raise */
    raise(108, 2),

    // ------------------------------------------------------------------------
    // New instructions in R10B.

    /** Implemented in org.jerlang.erts.emulator.op.BsBitsToBytes */
    bs_init2(109, 6),

    deprecated_bs_bits_to_bytes(110, 3),

    /** Implemented in org.jerlang.erts.emulator.op.BsAdd */
    bs_add(111, 5),

    /** Implemented in org.jerlang.erts.emulator.op.Apply */
    apply(112, 1),

    /** Implemented in org.jerlang.erts.emulator.op.ApplyLast */
    apply_last(113, 2),

    /** Implemented in org.jerlang.erts.emulator.op.IsBoolean */
    is_boolean(114, 2),

    // ------------------------------------------------------------------------
    // New instructions in R10B-6.

    /** Implemented in org.jerlang.erts.emulator.op.IsFunction2 */
    is_function2(115, 3),

    // ------------------------------------------------------------------------
    // New bit syntax matching in R11B.

    /** Implemented in org.jerlang.erts.emulator.op.BsStartMatch2 */
    bs_start_match2(116, 5),

    /** Implemented in org.jerlang.erts.emulator.op.BsGetInteger2 */
    bs_get_integer2(117, 7),

    /** Implemented in org.jerlang.erts.emulator.op.BsGetFloat2 */
    bs_get_float2(118, 7),

    /** Implemented in org.jerlang.erts.emulator.op.BsGetBinary2 */
    bs_get_binary2(119, 7),

    /** Implemented in org.jerlang.erts.emulator.op.BsSkipBits2 */
    bs_skip_bits2(120, 5),

    /** Implemented in org.jerlang.erts.emulator.op.BsTestTail2 */
    bs_test_tail2(121, 3),

    /** Implemented in org.jerlang.erts.emulator.op.BsSave2 */
    bs_save2(122, 2),

    /** Implemented in org.jerlang.erts.emulator.op.BsRestore */
    bs_restore2(123, 2),

    // ------------------------------------------------------------------------
    // New GC bifs introduced in R11B.

    /** Implemented in org.jerlang.erts.emulator.op.GcBif1 */
    gc_bif1(124, 5),

    /** Implemented in org.jerlang.erts.emulator.op.GcBif2 */
    gc_bif2(125, 6),

    // ------------------------------------------------------------------------
    // Experimental new bit_level bifs introduced in R11B.
    // NOT used in R12B.

    deprecated_bs_final2(126, 2),
    deprecated_bs_bits_to_bytes2(127, 2),

    // ------------------------------------------------------------------------
    // R11B-4

    deprecated_put_literal(128, 2),

    // ------------------------------------------------------------------------
    // R11B-5

    /** Implemented in org.jerlang.erts.emulator.op.IsBitstr */
    is_bitstr(129, 2),

    // ------------------------------------------------------------------------
    // R12B

    /** Implemented in org.jerlang.erts.emulator.op.BsContextToBinary */
    bs_context_to_binary(130, 1),

    /** Implemented in org.jerlang.erts.emulator.op.BsTestUnit */
    bs_test_unit(131, 3),

    /** Implemented in org.jerlang.erts.emulator.op.BsMatchString */
    bs_match_string(132, 4),

    /** Implemented in org.jerlang.erts.emulator.op.BsInitWritable */
    bs_init_writable(133, 0),

    /** Implemented in org.jerlang.erts.emulator.op.BsAppend */
    bs_append(134, 8),

    /** Implemented in org.jerlang.erts.emulator.op.BsPrivateAppend */
    bs_private_append(135, 6),

    /** Implemented in org.jerlang.erts.emulator.op.Trim */
    trim(136, 2),

    /** Implemented in org.jerlang.erts.emulator.op.BsInitBits */
    bs_init_bits(137, 6),

    // ------------------------------------------------------------------------
    // R12B-5

    /** Implemented in org.jerlang.erts.emulator.op.BsGetUtf8 */
    bs_get_utf8(138, 5),

    /** Implemented in org.jerlang.erts.emulator.op.BsSkipUtf8 */
    bs_skip_utf8(139, 4),

    /** Implemented in org.jerlang.erts.emulator.op.BsGetUtf16 */
    bs_get_utf16(140, 5),

    /** Implemented in org.jerlang.erts.emulator.op.BsSkipUtf16 */
    bs_skip_utf16(141, 4),

    /** Implemented in org.jerlang.erts.emulator.op.BsGetUtf32 */
    bs_get_utf32(142, 5),

    /** Implemented in org.jerlang.erts.emulator.op.BsSkipUtf32 */
    bs_skip_utf32(143, 4),

    /** Implemented in org.jerlang.erts.emulator.op.BsUtf8Size */
    bs_utf8_size(144, 3),

    /** Implemented in org.jerlang.erts.emulator.op.BsPutUtf8 */
    bs_put_utf8(145, 3),

    /** Implemented in org.jerlang.erts.emulator.op.BsUtf16Size */
    bs_utf16_size(146, 3),

    /** Implemented in org.jerlang.erts.emulator.op.BsPutUtf16 */
    bs_put_utf16(147, 3),

    /** Implemented in org.jerlang.erts.emulator.op.BsPutUtf32 */
    bs_put_utf32(148, 3),

    // ------------------------------------------------------------------------
    // R13B03

    /** Implemented in org.jerlang.erts.emulator.op.OnLoad */
    on_load(149, 0),

    // ------------------------------------------------------------------------
    // R14A

    /** Implemented in org.jerlang.erts.emulator.op.RecvMark */
    recv_mark(150, 1),

    /** Implemented in org.jerlang.erts.emulator.op.RecvSet */
    recv_set(151, 1),

    /** Implemented in org.jerlang.erts.emulator.op.GcBif3 */
    gc_bif3(152, 7),

    // ------------------------------------------------------------------------
    // R15A

    /** Implemented in org.jerlang.erts.emulator.op.Line */
    line(153, 1),

    // ------------------------------------------------------------------------
    // R17

    /** Implemented in org.jerlang.erts.emulator.op.PutMapAssoc */
    put_map_assoc(154, 5),

    /** Implemented in org.jerlang.erts.emulator.op.PutMapExact */
    put_map_exact(155, 5),

    /** Implemented in org.jerlang.erts.emulator.op.IsMap */
    is_map(156, 2),

    /** Implemented in org.jerlang.erts.emulator.op.HasMapFields */
    has_map_fields(157, 3),

    /** Implemented in org.jerlang.erts.emulator.op.GetMapElements */
    get_map_elements(158, 3);

    // ========================================================================

    private final int code;
    private final int arity;
    private final MethodHandle methodHandle;

    private Opcode(int code, int arity) {
        this.code = code;
        this.arity = arity;

        if (name().startsWith("deprecated")) {
            this.methodHandle = null;
            return;
        }

        String m = StringUtil.snakeToCamelCase(name());
        String c = "org.jerlang.erts.emulator.op." + m;
        MethodHandle mh = null;

        try {
            MethodType METHOD_TYPE = MethodType.methodType(
                Term.class,
                Process.class,
                Module.class,
                Instruction.class,
                List.class);
            Class<?> clazz = getClass().getClassLoader().loadClass(c);
            mh = MethodHandles.lookup().findStatic(clazz, "execute", METHOD_TYPE);
        } catch (NoSuchMethodException | IllegalAccessException | ClassNotFoundException e) {
            System.err.println("Can not export: " + c);
        }

        this.methodHandle = mh;
    }

    public Integer arity() {
        return arity;
    }

    public int encode() {
        return code;
    }

    public MethodHandle methodHandle() {
        return methodHandle;
    }

    @Override
    public String toString() {
        if (name().startsWith("_")) {
            return name().substring(1);
        } else {
            return name();
        }
    }

    public static Opcode decode(int value) {
        return values()[value - 1];
    }

    public static int max() {
        return values().length;
    }

}
