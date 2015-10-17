package org.jerlang.stdlib.lists;

import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;

public class ListsDuplicate {

    private ListsDuplicate() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 2:
            Integer n = params.head().toInteger();
            params = params.tail();
            Term elem = params.head();
            return duplicate_2(n, elem);
        default:
            throw Error.badarg;
        }
    }

    /**
     * Returns a list which contains N copies of the term Elem.
     *
     * http://www.erlang.org/doc/man/lists.html#duplicate-2
     */
    public static List duplicate_2(Integer n, Term elem) {
        // TODO: In Java, we could use a special data structure
        // TODO: that fakes a real list, but just contains a
        // TODO: reference to elem and a count of remaining n.
        // For now, we just use a normal list
        int elements = n.toInt();
        // TODO: check elements is positive
        List list = List.nil;
        while (elements-- > 0) {
            list = new List(elem, list);
        }
        return list;
    }

}
