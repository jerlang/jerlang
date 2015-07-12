package org.jerlang.erts.init;

import org.jerlang.type.Atom;

public class ProcessFlag {

    public static boolean process_flag(Atom flag, boolean value) {
        switch (flag.toString()) {
        case "trap_exit":
            return process_flag_trap_exit(value);
        default:
            // TODO: throw error
            return false;
        }
    }

    /**
     * When trap_exit is set to true, exit signals arriving to a process
     * are converted to {'EXIT', From, Reason} messages, which can be
     * received as ordinary messages. If trap_exit is set to false, the
     * process exits if it receives an exit signal other than normal and
     * the exit signal is propagated to its linked processes.
     * Application processes should normally not trap exits.
     *
     * Returns the old value of the flag.
     */
    private static boolean process_flag_trap_exit(boolean value) {
        return true; // TODO: implementation missing
    }

}
