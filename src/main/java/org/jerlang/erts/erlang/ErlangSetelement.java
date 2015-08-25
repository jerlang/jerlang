package org.jerlang.erts.erlang;

import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;

public class ErlangSetelement {

    private ErlangSetelement() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 3:
            Integer index = params.head().toInteger();
            params = params.tail();
            Tuple tuple = params.head().toTuple();
            params = params.tail();
            Term value = params.head();
            return setelement_3(index, tuple, value);
        default:
            throw new Error("badarg");
        }
    }

    /**
     * Returns a tuple which is a copy of the argument Tuple1
     * with the element given by the integer argument Index
     * (the first element is the element with index 1) replaced
     * by the argument Value.
     *
     * http://www.erlang.org/doc/man/erlang.html#setelement-3
     */
    public static Tuple setelement_3(Integer index, Tuple tuple, Term value) {
        Tuple copy = new Tuple(tuple);
        copy.set(index.toInt(), value);
        return copy;
    }

}
