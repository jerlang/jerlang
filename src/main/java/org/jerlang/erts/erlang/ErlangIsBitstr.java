package org.jerlang.erts.erlang;

import org.jerlang.type.BitString;
import org.jerlang.type.Boolean;
import org.jerlang.type.List;
import org.jerlang.type.Term;

public class ErlangIsBitstr {

    private ErlangIsBitstr() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 1:
            return is_bitstr_1(params.head());
        default:
            throw new Error("badarg");
        }
    }

    /**
     * http://www.erlang.org/doc/man/erlang.html#is_bitstring-1
     */
    public static Term is_bitstr_1(Term term) {
        return Boolean.of(term instanceof BitString);
    }

}
