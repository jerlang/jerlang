package org.jerlang.erts;

/**
 * http://www.erlang.org/doc/man/init.html
 */
public class Init {

    /**
     * boot(BootArgs) -> no_return()
     *
     * Types:
     * BootArgs = [binary()]
     *
     * Starts the Erlang runtime system.
     * This function is called when the emulator is started and
     * coordinates system start-up.
     *
     * BootArgs are all command line arguments except the emulator flags,
     * that is, flags and plain arguments. See erl(1).
     *
     * init itself interprets some of the flags, see Command Line Flags below.
     * The remaining flags ("user flags") and plain arguments are passed to the
     * init loop and can be retrieved by calling get_arguments/0 and
     * get_plain_arguments/0, respectively.
     * 
     * http://www.erlang.org/doc/man/init.html#boot-1
     */
    public static void boot(String[] args) {
        // called by OtpRing0 module
    }

}
