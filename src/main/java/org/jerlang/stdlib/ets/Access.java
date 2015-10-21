package org.jerlang.stdlib.ets;

import org.jerlang.type.Atom;

/**
 * ETS access rights.
 *
 * http://www.erlang.org/doc/man/ets.html
 */

public enum Access {

    PUBLIC("public"),
    PROTECTED("protected"),
    PRIVATE("private");

    private final Atom atom;
    private final String name;

    Access(String name) {
        this.atom = Atom.of(name);
        this.name = name;
    }

    public Atom toAtom() {
        return atom;
    }

    @Override
    public String toString() {
        return name;
    }

    public static Access byName(String name) {
        switch (name) {
        case "public":
            return PUBLIC;
        case "protected":
            return PROTECTED;
        case "private":
            return PRIVATE;
        default:
            return null;
        }
    }

}
