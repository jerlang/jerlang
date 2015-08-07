package org.jerlang.erts.erlang;

import org.jerlang.type.Atom;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * The minus operator
 */
public class ErlangAtomMinus {

    private ErlangAtomMinus() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 2:
            Integer a = params.head().toInteger();
            params = params.tail();
            Integer b = params.head().toInteger();
            return atom_minus_2(a, b);
        default:
            throw new Error("badarg");
        }
    }

    public static Integer atom_minus_2(Integer a, Integer b) {
        return new Integer(a.toBigInteger().subtract(b.toBigInteger()));
    }

    public static Atom name() {
        return Atom.of("-");
    }

}
