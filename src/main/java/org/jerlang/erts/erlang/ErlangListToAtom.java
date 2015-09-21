package org.jerlang.erts.erlang;

import org.jerlang.type.Atom;
import org.jerlang.type.List;
import org.jerlang.type.Str;
import org.jerlang.type.Term;

public class ErlangListToAtom {

    private ErlangListToAtom() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 1:
            return list_to_atom_1(params.head().toList());
        default:
            throw new Error("badarg");
        }
    }

    /**
     * Returns the atom whose text representation is String.
     *
     * http://www.erlang.org/doc/man/erlang.html#list_to_atom-1
     */
    public static Atom list_to_atom_1(List string) {
        if (string instanceof Str) {
            return Atom.of(string.toStr().string());
        } else {
            return Atom.of(Str.convert(string).string());
        }
    }

}
