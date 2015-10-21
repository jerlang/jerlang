package org.jerlang.stdlib.ets;

import org.jerlang.type.Atom;

/**
 * ETS types.
 *
 * http://www.erlang.org/doc/man/ets.html
 */
public enum Type {

    SET("set"),
    ORDERED_SET("ordered_set"),
    BAG("bag"),
    DUPLICATE_BAG("duplicate_bag");

    private final Atom atom;
    private final String name;

    Type(String name) {
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

    public static Type byName(String name) {
        switch (name) {
        case "set":
            return SET;
        case "ordered_set":
            return ORDERED_SET;
        case "bag":
            return BAG;
        case "duplicate_bag":
            return DUPLICATE_BAG;
        default:
            return null;
        }
    }

}
