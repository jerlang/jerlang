package org.jerlang;

import org.jerlang.type.Atom;

/**
 * Defines the process priority.
 * The value is equal to the ordinal of the enum.
 *
 * See:
 * http://erlang.org/doc/man/erlang.html#process_flag-2
 * http://jlouisramblings.blogspot.de/2013/01/how-erlang-does-scheduling.html
 */
public enum ProcessPriority {

    MAX,
    HIGH,
    NORMAL,
    LOW;

    public Atom toAtom() {
        return Atom.of(name().toLowerCase());
    }

}
