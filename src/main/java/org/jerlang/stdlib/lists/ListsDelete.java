package org.jerlang.stdlib.lists;

import org.jerlang.erts.erlang.Error;
import org.jerlang.stdlib.Lists;
import org.jerlang.type.List;
import org.jerlang.type.Term;

public class ListsDelete {

    private ListsDelete() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 2:
            Term element = params.head();
            params = params.tail();
            List list = params.head().toList();
            return delete_2(element, list);
        default:
            throw new Error("badarg");
        }
    }

    /**
     * Returns a copy of List1 where the first element matching is deleted,
     * if there is such an element.
     *
     * http://www.erlang.org/doc/man/lists.html#delete-2
     */
    public static List delete_2(Term element, List list) {
        List result = List.nil;
        boolean deleted = false;
        while (list.length() > 0) {
            if (!deleted && list.head().equals(element)) {
                deleted = true;
            } else {
                result = new List(list.head(), result);
            }
            list = list.tail();
        }
        return Lists.reverse(result);
    }

}
