package org.jerlang.stdlib.lists;

import org.jerlang.erts.Erlang;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.List;
import org.jerlang.type.Term;

public class ListsReverse {

    private ListsReverse() {
    }

    public static Term dispatch(List params) {
        switch (Erlang.length(params).toInt()) {
        case 1:
            return reverse_1(params.head().toList());
        default:
            throw new Error("badarg");
        }
    }

    /**
     * Returns a list with the elements in `list` in reverse order.
     *
     * http://www.erlang.org/doc/man/lists.html#reverse-1
     */
    public static List reverse_1(List list) {
        if (list.equals(List.nil)) {
            return list;
        }
        if (list.tail().equals(List.nil)) {
            return list;
        }
        List result = List.nil;
        while (!list.equals(List.nil)) {
            result = new List(list.head(), result);
            list = list.tail();
        }
        return result;
    }

}
