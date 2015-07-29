package org.jerlang.erts.erlang;

import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;

public class ErlangTupleToList {

    private ErlangTupleToList() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 1:
            Tuple tuple = params.head().toTuple();
            return tuple_to_list_1(tuple);
        default:
            throw new Error("badarg");
        }
    }

    /**
     * Returns a list which corresponds to Tuple.
     * Tuple may contain any Erlang terms.
     */
    public static List tuple_to_list_1(Tuple tuple) {
        List list = new List();
        for (int index = tuple.arity() - 1; index >= 0; index--) {
            list = new List(tuple.element(index + 1), list);
        }
        return list;
    }

}
