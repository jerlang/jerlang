package org.jerlang;

/**
 * Based on:
 * https://github.com/erlang/otp/blob/master/lib/compiler/src/genop.tab
 */
public enum Opcode {

    /**
     * Specify a module local label.
     * Label gives this code address a name (Lbl) and marks the start of
     * a basic block.
     */
    label(1),

    /**
     * Define a function M:F/A
     */
    func_info(2),

    int_code_end(3),

    // ========================================================================
    // Function and BIF calls.

    /**
     * Call the function at Label.
     * Save the next instruction as the return address in the CP register.
     */
    call(4),

    /**
     * Deallocate and do a tail recursive call to the function at Label.
     * Do not update the CP register.
     * Before the call deallocate Deallocate words of stack.
     */
    call_last(5),

    /**
     * Do a tail recursive call to the function at Label.
     * Do not update the CP register.
     */
    call_only(6),

    /**
     * Call the function of arity Arity pointed to by Destination.
     * Save the next instruction as the return address in the CP register.
     */
    call_ext(7),

    /**
     * Deallocate and do a tail call to function of arity Arity
     * pointed to by Destination.
     * Do not update the CP register.
     * Deallocate Deallocate words from the stack before the call.
     */
    call_ext_last(8),

    /**
     * Call the bif Bif and store the result in Reg.
     */
    bif0(9),

    /**
     * Call the bif Bif with the argument Arg, and store the result in Reg.
     * On failure jump to Lbl.
     */
    bif1(10),

    /**
     * Call the bif Bif with the arguments Arg1 and Arg2,
     * and store the result in Reg.
     * On failure jump to Lbl.
     */
    bif2(11),

    // ========================================================================
    // Allocating, deallocating and returning.

    /**
     * Allocate space for StackNeed words on the stack. If a GC is needed
     * during allocation there are Live number of live X registers.
     * Also save the continuation pointer (CP) on the stack.
     */
    allocate(12),

    /**
     * Allocate space for StackNeed words on the stack and ensure there is
     * space for HeapNeed words on the heap. If a GC is needed
     * save Live number of X registers.
     * Also save the continuation pointer (CP) on the stack.
     */
    allocate_heap(13),

    /**
     * Allocate space for StackNeed words on the stack. If a GC is needed
     * during allocation there are Live number of live X registers.
     * Clear the new stack words. (By writing NIL.)
     * Also save the continuation pointer (CP) on the stack.
     */
    allocate_zero(14),

    /**
     * Allocate space for StackNeed words on the stack and HeapNeed words
     * on the heap. If a GC is needed
     * during allocation there are Live number of live X registers.
     * Clear the new stack words. (By writing NIL.)
     * Also save the continuation pointer (CP) on the stack.
     */
    allocate_heap_zero(15),

    /**
     * Ensure there is space for HeapNeed words on the heap. If a GC is needed
     * save Live number of X registers.
     */
    test_heap(16),

    /**
     * Clear the Nth stack word. (By writing NIL.)
     */
    init(17),

    /**
     * Restore the continuation pointer (CP) from the stack and deallocate
     * N+1 words from the stack (the + 1 is for the CP).
     */
    deallocate(18),

    /**
     * Return to the address in the continuation pointer (CP).
     */
    _return(19),

    // ========================================================================
    // Sending & receiving.

    /**
     * Send argument in x(0) as a message to the destination process in x(0).
     *  The message in x(1) ends up as the result of the send in x(0).
     */
    send(20),

    /**
     * Unlink the current message from the message queue and store a
     *  pointer to the message in x(0). Remove any timeout.
     */
    remove_message(21),

    /**
     * Reset the save point of the mailbox and clear the timeout flag.
     */
    timeout(22),

    /**
     * Loop over the message queue, if it is empty jump to Label.
     */
    loop_rec(23),

    /**
     * Advance the save pointer to the next message and jump back to Label.
     */
    loop_rec_end(24),

    /**
     * Suspend the processes and set the entry point to the beginning of the
     *  receive loop at Label.
     */
    wait(25),

    /**
     * Sets up a timeout of Time milllisecons and saves the address of the
     * following instruction as the entry point if the timeout triggers.
     */
    wait_timeout(26),

    // ========================================================================
    // Arithmethic opcodes.

    m_plus(27),
    m_minus(28),
    m_times(29),
    m_div(30),
    int_div(31),
    int_rem(32),
    int_band(33),
    int_bor(34),
    int_bxor(35),
    int_bsl(36),
    int_bsr(37),
    int_bnot(38),

    // ========================================================================
    // Comparision operators.

    /**
     * Compare two terms and jump to Lbl if Arg1 is not less than Arg2.
     */
    is_lt(39),

    /**
     * Compare two terms and jump to Lbl if Arg1 is less than Arg2.
     */
    is_ge(40),

    /**
     * Compare two terms and jump to Lbl if Arg1 is not (numerically) equal to Arg2.
     */
    is_eq(41),

    /**
     * Compare two terms and jump to Lbl if Arg1 is (numerically) equal to Arg2.
     */
    is_ne(42),

    /**
     * Compare two terms and jump to Lbl if Arg1 is not exactly equal to Arg2.
     */
    is_eq_exact(43),

    /**
     * Compare two terms and jump to Lbl if Arg1 is exactly equal to Arg2.
     */
    is_ne_exact(44),

    // ========================================================================
    // Type tests.

    /**
     * Test the type of Arg1 and jump to Lbl if it is not an integer.
     */
    is_integer(45),

    /**
     * Test the type of Arg1 and jump to Lbl if it is not a float.
     */
    is_float(46),

    /**
     * Test the type of Arg1 and jump to Lbl if it is not a number.
     */
    is_number(47),

    /**
     * Test the type of Arg1 and jump to Lbl if it is not an atom.
     */
    is_atom(48),

    /**
     * Test the type of Arg1 and jump to Lbl if it is not a pid.
     */
    is_pid(49),

    /**
     * Test the type of Arg1 and jump to Lbl if it is not a reference.
     */
    is_reference(50),

    /**
     * Test the type of Arg1 and jump to Lbl if it is not a port.
     */
    is_port(51),

    /**
     * Test the type of Arg1 and jump to Lbl if it is not nil.
     */
    is_nil(52),

    /**
     * Test the type of Arg1 and jump to Lbl if it is not a binary.
     */
    is_binary(53),

    is_constant(54),

    /**
     * Test the type of Arg1 and jump to Lbl if it is not a cons or nil.
     */
    is_list(55),

    /**
     * Test the type of Arg1 and jump to Lbl if it is not a cons.
     */
    is_nonempty_list(56),

    /**
     * Test the type of Arg1 and jump to Lbl if it is not a tuple.
     */
    is_tuple(57),

    /**
     * Test the arity of (the tuple in) Arg1 and jump
     */
    // to Lbl if it is not equal to Arity.
    test_arity(58),

    // ========================================================================
    // Indexing & jumping.

    /**
     * Jump to the destination label corresponding to Arg
     * in the Destinations list, if no arity matches, jump to FailLabel.
     */
    select_val(59),

    /**
     * Check the arity of the tuple Tuple and jump to the corresponding
     * destination label, if no arity matches, jump to FailLabel.
     */
    select_tuple_arity(60),

    /**
     * Jump to Label.
     */
    jump(61),

    // ========================================================================
    // Catch.

    _catch(62),
    catch_end(63),

    // ========================================================================
    // Moving, extracting, modifying.

    /**
     * Move the source Source (a literal or a register) to
     * the destination register Destination.
     */
    move(64),

    /**
     * Get the head and tail (or car and cdr) parts of a list
     * (a cons cell) from Source and put them into the registers
     * Head and Tail.
     */
    get_list(65),

    /**
     * Get element number Element from the tuple in Source and put
     * it in the destination register Destination.
     */
    get_tuple_element(66),

    /**
     * Update the element at postition Position of the tuple Tuple
     * with the new element NewElement.
     */
    set_tuple_element(67),

    // ========================================================================
    // Building terms.

    put_string(68),
    put_list(69),
    put_tuple(70),
    put(71),

    // ========================================================================
    // Raising errors.

    badmatch(72),
    if_end(73),
    case_end(74),

    // ========================================================================
    // 'fun' support.

    /**
     * Call a fun of arity Arity. Assume arguments in
     * registers x(0) to x(Arity-1) and that the fun is in x(Arity).
     * Save the next instruction as the return address in the CP register.
     */
    call_fun(75),

    make_fun(76),

    /**
     * Test the type of Arg1 and jump to Lbl if it is not a
     * function (i.e. fun or closure).
     */
    is_function(77),

    // ========================================================================
    // Late additions to R5.

    /**
     * Do a tail recursive call to the function at Label.
     * Do not update the CP register.
     */
    call_ext_only(78),

    // ========================================================================
    // Binary matching (R7).

    bs_start_match(79),
    bs_get_integer(80),
    bs_get_float(81),
    bs_get_binary(82),
    bs_skip_bits(83),
    bs_test_tail(84),
    bs_save(85),
    bs_restore(86),

    // ========================================================================
    // Binary construction (R7A).

    bs_init(87),
    bs_final(88),
    bs_put_integer(89),
    bs_put_binary(90),
    bs_put_float(91),
    bs_put_string(92),

    // ========================================================================
    // Binary construction (R7B).

    bs_need_buf(93),

    // ========================================================================
    // Floating point arithmetic (R8).

    fclearerror(94),
    fcheckerror(95),
    fmove(96),
    fconv(97),
    fadd(98),
    fsub(99),
    fmul(100),
    fdiv(101),
    fnegate(102),

    // ========================================================================
    // New fun construction (R8).

    make_fun2(103),

    // ========================================================================
    // Try/catch/raise (R10B).

    _try(104),
    try_end(105),
    try_case(106),
    try_case_end(107),
    raise(108),

    // ========================================================================
    // New instructions in R10B.

    bs_init2(109),
    bs_bits_to_bytes(110),
    bs_add(111),
    apply(112),
    apply_last(113),

    /**
     * Test the type of Arg1 and jump to Lbl if it is not a Boolean.
     */
    is_boolean(114),

    // ========================================================================
    // New instructions in R10B-6.

    /**
     * Test the type of Arg1 and jump to Lbl if it is not a
     * function of arity Arity.
     */
    is_function2(115),

    // ========================================================================
    // New bit syntax matching in R11B.

    bs_start_match2(116),
    bs_get_integer2(117),
    bs_get_float2(118),
    bs_get_binary2(119),
    bs_skip_bits2(120),
    bs_test_tail2(121),
    bs_save2(122),
    bs_restore2(123),

    // ========================================================================
    // New GC bifs introduced in R11B.

    /**
     * Call the bif Bif with the argument Arg, and store the result in Reg.
     * On failure jump to Lbl.
     * Do a garbage collection if necessary to allocate space on the heap
     * for the result (saving Live number of X registers).
     */
    gc_bif1(124),

    /**
     * Call the bif Bif with the arguments Arg1 and Arg2,
     * and store the result in Reg.
     * On failure jump to Lbl.
     * Do a garbage collection if necessary to allocate space on the heap
     * for the result (saving Live number of X registers).
     */
    gc_bif2(125),

    /**
     * Experimental new bit_level bifs introduced in R11B.
     */
    // NOT used in R12B.
    bs_final2(126),
    bs_bits_to_bytes2(127),

    // ========================================================================
    // R11B-4

    put_literal(128),

    // ========================================================================
    // R11B-5

    /**
     * Test the type of Arg1 and jump to Lbl if it is not a bit string.
     */
    is_bitstr(129),

    // ========================================================================
    // R12B

    bs_context_to_binary(130),
    bs_test_unit(131),
    bs_match_string(132),
    bs_init_writable(133),
    bs_append(134),
    bs_private_append(135),

    /**
     * Reduce the stack usage by N words,
     * keeping the CP on the top of the stack.
     */
    trim(136),

    bs_init_bits(137),

    // ========================================================================
    // R12B-5

    bs_get_utf8(138),
    bs_skip_utf8(139),

    bs_get_utf16(140),
    bs_skip_utf16(141),

    bs_get_utf32(142),
    bs_skip_utf32(143),

    bs_utf8_size(144),
    bs_put_utf8(145),

    bs_utf16_size(146),
    bs_put_utf16(147),

    bs_put_utf32(148),

    // ========================================================================
    // R13B03

    on_load(149),

    // ========================================================================
    // R14A

    /**
     * Save the end of the message queue and the address of
     * the label Label so that a recv_set instruction can start
     * scanning the inbox from this position.
     */
    recv_mark(150),

    /**
     * Check that the saved mark points to Label and set the
     * save pointer in the message queue to the last position
     * of the message queue saved by the recv_mark instruction.
     */
    recv_set(151),

    /**
     * Call the bif Bif with the arguments Arg1, Arg2 and Arg3,
     * and store the result in Reg.
     * On failure jump to Lbl.
     * Do a garbage collection if necessary to allocate space on the heap
     * for the result (saving Live number of X registers).
     */
    gc_bif3(152),

    // ========================================================================
    // R15A

    line(153),

    // ========================================================================
    // R17

    put_map_assoc(154),
    put_map_exact(155),
    is_map(156),
    has_map_fields(157),
    get_map_elements(158);

    private final int code;

    private Opcode(int code) {
        this.code = code;
    }

    public int encode() {
        return code;
    }

    public static Opcode decode(int value) {
        // TODO: Optimize lookup
        for (Opcode opcode : values()) {
            if (opcode.code == value) {
                return opcode;
            }
        }
        return null;
    }

    @Override
    public String toString() {
        if (name().startsWith("_")) {
            return name().substring(1);
        } else {
            return name();
        }
    }

}
