package org.jerlang.erts.erlang;

import org.jerlang.stdlib.Lists;
import org.jerlang.type.Atom;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Concatenate two lists
 */
public class ErlangAtomPlusPlus {

    private ErlangAtomPlusPlus() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 2:
            List a = params.head().toList();
            params = params.tail();
            List b = params.head().toList();
            return atom_plus_plus_2(a, b);
        default:
            throw new Error("badarg");
        }
    }

    public static List atom_plus_plus_2(List a, List b) {
        return Lists.append(a, b);
    }

    public static Atom name() {
        return Atom.of("++");
    }

}
