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

    /**
     * Specify a module local label.
     * Label gives this code address a name (Lbl)
     * and marks the start of a basic block.
     */
    label(1, 1),

    /**
     * Define a function M:F/A
     */
    func_info(2, 3),

    /**
     * Marker for end of code.
     * This is the last opcode in a code chunk.
     */
    int_code_end(3, 0),

    // ========================================================================
    // Function and BIF calls.

    /**
     * Call the function at Label.
     * Save the next instruction as the return address in the CP register.
     */
    call(4, 2),

    /**
     * Deallocate and do a tail recursive call to the function at Label.
     * Do not update the CP register.
     * Before the call deallocate Deallocate words of stack.
     */
    call_last(5, 3),

    /**
     * Do a tail recursive call to the function at Label.
     * Do not update the CP register.
     */
    call_only(6, 2),

    /**
     * Call the function of arity Arity pointed to by Destination.
     * Save the next instruction as the return address in the CP register.
     *
     * First argument is the arity.
     * Second argument is the index of the function in import chunk.
     */
    call_ext(7, 2),

    /**
     * Deallocate and do a tail call to function of arity Arity
     * pointed to by Destination.
     * Do not update the CP register.
     * Deallocate Deallocate words from the stack before the call.
     */
    call_ext_last(8, 3),

    /**
     * Call the bif Bif and store the result in Reg.
     */
    bif0(9, 2),

    /**
     * Call the bif Bif with the argument Arg, and store the result in Reg.
     * On failure jump to Lbl.
     */
    bif1(10, 4),

    /**
     * Call the bif Bif with the arguments Arg1 and Arg2,
     * and store the result in Reg.
     * On failure jump to Lbl.
     */
    bif2(11, 5),

    // ========================================================================
    // Allocating, deallocating and returning.

    /**
     * Allocate space for StackNeed words on the stack. If a GC is needed
     * during allocation there are Live number of live X registers.
     * Also save the continuation pointer (CP) on the stack.
     */
    allocate(12, 2),

    /**
     * Allocate space for StackNeed words on the stack and ensure there is
     * space for HeapNeed words on the heap. If a GC is needed
     * save Live number of X registers.
     * Also save the continuation pointer (CP) on the stack.
     */
    allocate_heap(13, 3),

    /**
     * Allocate space for StackNeed words on the stack. If a GC is needed
     * during allocation there are Live number of live X registers.
     * Clear the new stack words. (By writing NIL.)
     * Also save the continuation pointer (CP) on the stack.
     */
    allocate_zero(14, 2),

    /**
     * Allocate space for StackNeed words on the stack and HeapNeed words
     * on the heap. If a GC is needed
     * during allocation there are Live number of live X registers.
     * Clear the new stack words. (By writing NIL.)
     * Also save the continuation pointer (CP) on the stack.
     */
    allocate_heap_zero(15, 3),

    /**
     * Ensure there is space for HeapNeed words on the heap. If a GC is needed
     * save Live number of X registers.
     */
    test_heap(16, 2),

    /**
     * Clear the Nth stack word. (By writing NIL.)
     */
    init(17, 1),

    /**
     * Restore the continuation pointer (CP) from the stack and deallocate
     * N+1 words from the stack (the + 1 is for the CP).
     */
    deallocate(18, 1),

    /**
     * Return to the address in the continuation pointer (CP).
     */
    _return(19, 0),

    // ========================================================================
    // Sending & receiving.

    /**
     * Send argument in x(0) as a message to the destination process in x(0).
     *  The message in x(1) ends up as the result of the send in x(0).
     */
    send(20, 0),

    /**
     * Unlink the current message from the message queue and store a
     *  pointer to the message in x(0). Remove any timeout.
     */
    remove_message(21, 0),

    /**
     * Reset the save point of the mailbox and clear the timeout flag.
     */
    timeout(22, 0),

    /**
     * Loop over the message queue, if it is empty jump to Label.
     */
    loop_rec(23, 2),

    /**
     * Advance the save pointer to the next message and jump back to Label.
     */
    loop_rec_end(24, 1),

    /**
     * Suspend the processes and set the entry point to the beginning of the
     *  receive loop at Label.
     */
    wait(25, 1),

    /**
     * Sets up a timeout of Time milllisecons and saves the address of the
     * following instruction as the entry point if the timeout triggers.
     */
    wait_timeout(26, 2),

    // ========================================================================
    // Arithmethic opcodes.

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

    // ========================================================================
    // Comparision operators.

    /**
     * Compare two terms and jump to Lbl if Arg1 is not less than Arg2.
     */
    is_lt(39, 3),

    /**
     * Compare two terms and jump to Lbl if Arg1 is less than Arg2.
     */
    is_ge(40, 3),

    /**
     * Compare two terms and jump to Lbl if Arg1 is not (numerically) equal to Arg2.
     */
    is_eq(41, 3),

    /**
     * Compare two terms and jump to Lbl if Arg1 is (numerically) equal to Arg2.
     */
    is_ne(42, 3),

    /**
     * Compare two terms and jump to Lbl if Arg1 is not exactly equal to Arg2.
     */
    is_eq_exact(43, 3),

    /**
     * Compare two terms and jump to Lbl if Arg1 is exactly equal to Arg2.
     */
    is_ne_exact(44, 3),

    // ========================================================================
    // Type tests.

    /**
     * Test the type of Arg1 and jump to Lbl if it is not an integer.
     */
    is_integer(45, 2),

    /**
     * Test the type of Arg1 and jump to Lbl if it is not a float.
     */
    is_float(46, 2),

    /**
     * Test the type of Arg1 and jump to Lbl if it is not a number.
     */
    is_number(47, 2),

    /**
     * Test the type of Arg1 and jump to Lbl if it is not an atom.
     */
    is_atom(48, 2),

    /**
     * Test the type of Arg1 and jump to Lbl if it is not a pid.
     */
    is_pid(49, 2),

    /**
     * Test the type of Arg1 and jump to Lbl if it is not a reference.
     */
    is_reference(50, 2),

    /**
     * Test the type of Arg1 and jump to Lbl if it is not a port.
     */
    is_port(51, 2),

    /**
     * Test the type of Arg1 and jump to Lbl if it is not nil.
     */
    is_nil(52, 2),

    /**
     * Test the type of Arg1 and jump to Lbl if it is not a binary.
     */
    is_binary(53, 2),

    deprecated_is_constant(54, 2),

    /**
     * Test the type of Arg1 and jump to Lbl if it is not a cons or nil.
     */
    is_list(55, 2),

    /**
     * Test the type of Arg1 and jump to Lbl if it is not a cons.
     */
    is_nonempty_list(56, 2),

    /**
     * Test the type of Arg1 and jump to Lbl if it is not a tuple.
     */
    is_tuple(57, 2),

    /**
     * Test the arity of (the tuple in) Arg1 and jump
     */
    // to Lbl if it is not equal to Arity.
    test_arity(58, 3),

    // ========================================================================
    // Indexing & jumping.

    /**
     * Jump to the destination label corresponding to Arg
     * in the Destinations list, if no arity matches, jump to FailLabel.
     */
    select_val(59, 3),

    /**
     * Check the arity of the tuple Tuple and jump to the corresponding
     * destination label, if no arity matches, jump to FailLabel.
     */
    select_tuple_arity(60, 3),

    /**
     * Jump to Label.
     */
    jump(61, 1),

    // ========================================================================
    // Catch.

    _catch(62, 2),
    catch_end(63, 1),

    // ========================================================================
    // Moving, extracting, modifying.

    /**
     * Move the source Source (a literal or a register) to
     * the destination register Destination.
     */
    move(64, 2),

    /**
     * Get the head and tail (or car and cdr) parts of a list
     * (a cons cell) from Source and put them into the registers
     * Head and Tail.
     */
    get_list(65, 3),

    /**
     * Get element number Element from the tuple in Source and put
     * it in the destination register Destination.
     */
    get_tuple_element(66, 3),

    /**
     * Update the element at postition Position of the tuple Tuple
     * with the new element NewElement.
     */
    set_tuple_element(67, 3),

    // ========================================================================
    // Building terms.

    deprecated_put_string(68, 3),
    put_list(69, 3),
    put_tuple(70, 2),
    put(71, 1),

    // ========================================================================
    // Raising errors.

    badmatch(72, 1),
    if_end(73, 0),
    case_end(74, 1),

    // ========================================================================
    // 'fun' support.

    /**
     * Call a fun of arity Arity. Assume arguments in
     * registers x(0) to x(Arity-1) and that the fun is in x(Arity).
     * Save the next instruction as the return address in the CP register.
     */
    call_fun(75, 1),

    deprecated_make_fun(76, 3),

    /**
     * Test the type of Arg1 and jump to Lbl if it is not a
     * function (i.e. fun or closure).
     */
    is_function(77, 2),

    // ========================================================================
    // Late additions to R5.

    /**
     * Do a tail recursive call to the function at Label.
     * Do not update the CP register.
     */
    call_ext_only(78, 2),

    // ========================================================================
    // Binary matching (R7).

    deprecated_bs_start_match(79, 2),
    deprecated_bs_get_integer(80, 5),
    deprecated_bs_get_float(81, 5),
    deprecated_bs_get_binary(82, 5),
    deprecated_bs_skip_bits(83, 4),
    deprecated_bs_test_tail(84, 2),
    deprecated_bs_save(85, 1),
    deprecated_bs_restore(86, 1),

    // ========================================================================
    // Binary construction (R7A).

    deprecated_bs_init(87, 2),
    deprecated_bs_final(88, 2),
    bs_put_integer(89, 5),
    bs_put_binary(90, 5),
    bs_put_float(91, 5),
    bs_put_string(92, 2),

    // ========================================================================
    // Binary construction (R7B).

    deprecated_bs_need_buf(93, 1),

    // ========================================================================
    // Floating point arithmetic (R8).

    fclearerror(94, 0),
    fcheckerror(95, 1),
    fmove(96, 2),
    fconv(97, 2),
    fadd(98, 4),
    fsub(99, 4),
    fmul(100, 4),
    fdiv(101, 4),
    fnegate(102, 3),

    // ========================================================================
    // New fun construction (R8).

    make_fun2(103, 1),

    // ========================================================================
    // Try/catch/raise (R10B).

    _try(104, 2),
    try_end(105, 1),
    try_case(106, 1),
    try_case_end(107, 1),
    raise(108, 2),

    // ========================================================================
    // New instructions in R10B.

    bs_init2(109, 6),
    deprecated_bs_bits_to_bytes(110, 3),
    bs_add(111, 5),
    apply(112, 1),
    apply_last(113, 2),

    /**
     * Test the type of Arg1 and jump to Lbl if it is not a Boolean.
     */
    is_boolean(114, 2),

    // ========================================================================
    // New instructions in R10B-6.

    /**
     * Test the type of Arg1 and jump to Lbl if it is not a
     * function of arity Arity.
     */
    is_function2(115, 3),

    // ========================================================================
    // New bit syntax matching in R11B.

    bs_start_match2(116, 5),
    bs_get_integer2(117, 7),
    bs_get_float2(118, 7),
    bs_get_binary2(119, 7),
    bs_skip_bits2(120, 5),
    bs_test_tail2(121, 3),
    bs_save2(122, 2),
    bs_restore2(123, 2),

    // ========================================================================
    // New GC bifs introduced in R11B.

    /**
     * Call the bif Bif with the argument Arg, and store the result in Reg.
     * On failure jump to Lbl.
     * Do a garbage collection if necessary to allocate space on the heap
     * for the result (saving Live number of X registers).
     */
    gc_bif1(124, 5),

    /**
     * Call the bif Bif with the arguments Arg1 and Arg2,
     * and store the result in Reg.
     * On failure jump to Lbl.
     * Do a garbage collection if necessary to allocate space on the heap
     * for the result (saving Live number of X registers).
     */
    gc_bif2(125, 6),

    /**
     * Experimental new bit_level bifs introduced in R11B.
     */
    // NOT used in R12B.
    deprecated_bs_final2(126, 2),
    deprecated_bs_bits_to_bytes2(127, 2),

    // ========================================================================
    // R11B-4

    deprecated_put_literal(128, 2),

    // ========================================================================
    // R11B-5

    /**
     * Test the type of Arg1 and jump to Lbl if it is not a bit string.
     */
    is_bitstr(129, 2),

    // ========================================================================
    // R12B

    bs_context_to_binary(130, 1),
    bs_test_unit(131, 3),
    bs_match_string(132, 4),
    bs_init_writable(133, 0),
    bs_append(134, 8),
    bs_private_append(135, 6),

    /**
     * Reduce the stack usage by N words,
     * keeping the CP on the top of the stack.
     */
    trim(136, 2),

    bs_init_bits(137, 6),

    // ========================================================================
    // R12B-5

    bs_get_utf8(138, 5),
    bs_skip_utf8(139, 4),

    bs_get_utf16(140, 5),
    bs_skip_utf16(141, 4),

    bs_get_utf32(142, 5),
    bs_skip_utf32(143, 4),

    bs_utf8_size(144, 3),
    bs_put_utf8(145, 3),

    bs_utf16_size(146, 3),
    bs_put_utf16(147, 3),

    bs_put_utf32(148, 3),

    // ========================================================================
    // R13B03

    on_load(149, 0),

    // ========================================================================
    // R14A

    /**
     * Save the end of the message queue and the address of
     * the label Label so that a recv_set instruction can start
     * scanning the inbox from this position.
     */
    recv_mark(150, 1),

    /**
     * Check that the saved mark points to Label and set the
     * save pointer in the message queue to the last position
     * of the message queue saved by the recv_mark instruction.
     */
    recv_set(151, 1),

    /**
     * Call the bif Bif with the arguments Arg1, Arg2 and Arg3,
     * and store the result in Reg.
     * On failure jump to Lbl.
     * Do a garbage collection if necessary to allocate space on the heap
     * for the result (saving Live number of X registers).
     */
    gc_bif3(152, 7),

    // ========================================================================
    // R15A

    line(153, 1),

    // ========================================================================
    // R17

    put_map_assoc(154, 5),
    put_map_exact(155, 5),
    is_map(156, 2),
    has_map_fields(157, 3),
    get_map_elements(158, 3);

    private final int code;
    private final int arity;
    private final MethodHandle methodHandle;

    private Opcode(int code, int arity) {
        this.code = code;
        this.arity = arity;

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
            // System.err.println("Can not export: " + c);
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
