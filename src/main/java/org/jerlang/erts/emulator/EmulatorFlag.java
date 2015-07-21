package org.jerlang.erts.emulator;

public enum EmulatorFlag {

    /**
     * Suggested stack size, in kilowords, for threads in the async-thread pool.
     * Valid range is 16-8192 kilowords.
     * The default suggested stack size is 16 kilowords,
     * i.e, 64 kilobyte on 32-bit architectures.
     * This small default size has been chosen since the amount of async-threads
     * might be quite large. The default size is enough for drivers delivered
     * with Erlang/OTP, but might not be sufficiently large for other
     * dynamically linked in drivers that use the driver_async()
     * functionality. Note that the value passed is only a suggestion,
     * and it might even be ignored on some platforms.
     */
    STACK_SIZE("+a"),

    /**
     * Sets the number of threads in async thread pool, valid range is 0-1024.
     * If thread support is available, the default is 10.
     */
    ASYNC_THREAD_POOL("+A"),

    /**
     * The c option makes Ctrl-C interrupt the current shell instead of
     * invoking the emulator break handler.
     * The d option (same as specifying +B without an extra option)
     * disables the break handler.
     * The i option makes the emulator ignore any break signal.
     *
     * If the c option is used with oldshell on Unix,
     * Ctrl-C will restart the shell process rather than interrupt it.
     *
     * Note that on Windows, this flag is only applicable for werl,
     * not erl (oldshell).
     * Note also that Ctrl-Break is used instead of Ctrl-C on Windows.
     */
    BREAK("+B"),

    /**
     * Enable or disable time correction:
     *
     * true::
     * Enable time correction.
     * This is the default if time correction is supported on
     * the specific platform.
     *
     * false::
     * Disable time correction.
     *
     * For backwards compatibility, the boolean value can be omitted.
     * This is interpreted as +c false.
     */
    TIME_CORRECTION("+c"),

    TIME_WARP("+C"),
    DUMP("+d"),
    ETS_TABLES("+e"),
    ETS_COMPRESSED("+ec"),
    FILENAME_LATIN("+fnl"),
    FILENAME_UTF8("+fnu"),
    FILENAME_AUTO("+fna"),
    HEAP_SIZE("+hms"),
    HEAP_SIZE_BINARY("+hmbs"),
    PROCESS_DICTIONARY_SIZE("+hpds"),
    KERNEL_POLL("+K"),
    LOAD_TRACING("+l"),
    DONT_LOAD_LINES("+L"),
    MFLAG("+MFlag"),
    PORT_SIGNAL_BEHAVIOUR("+n"),
    PRINTABLE_CHARACTER("+pc"),
    MAX_PROCESSES("+P"),
    MAX_PORTS("+Q"),
    RELEASE("+R"),
    REALLOC("+r"),
    READER_GROUPS("+rg"),
    SCHEDULERS("+S"),
    SCHEDULERS_PERCENT("+SP"),
    SCHEDULERS_DIRTY_CPU("+SDcpu"),
    SCHEDULERS_DIRTY_CPU_PERCENT("+SDPcpu"),
    SCHEDULERS_DIRTY_IO("+SDio"),
    SCHEDULER_FLAG("+sFlag"),
    MAX_ATOMS("+t"),
    TIMING_LEVEL("+T"),
    VERSION1("+V"),
    VERSION2("-version"),
    VERBOSE("+v"),
    WARNING("+W"),
    MISC_FLAG("+zFlag"),

    /**
     * Selects an instrumented Erlang runtime system (virtual machine) to run,
     * instead of the ordinary one.
     * When running an instrumented runtime system, some resource usage data
     * can be obtained and analysed using the module instrument.
     * Functionally, it behaves exactly like an ordinary Erlang runtime system.
     */
    INSTRUMENT("-instr");

    private final String flag;

    private EmulatorFlag(String flag) {
        this.flag = flag;
    }

    @Override
    public String toString() {
        return flag;
    }

    public static boolean valid(String string) {
        return of(string) != null;
    }

    public static EmulatorFlag of(String string) {
        for (EmulatorFlag flag : values()) {
            if (flag.toString().equals(string)) {
                return flag;
            }
        }
        return null;
    }

}
