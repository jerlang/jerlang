package org.jerlang.erts.erlang;

import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;

public class ErlangTupleSize {

    private ErlangTupleSize() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 1:
            Tuple tuple = params.head().toTuple();
            return tuple_size_1(tuple);
        default:
            throw new Error("badarg");
        }
    }

    /**
     * Returns an integer which is the number of elements in `tuple`.
     */
    public static Integer tuple_size_1(Tuple tuple) {
        return Integer.of(tuple.arity());
    }

}
