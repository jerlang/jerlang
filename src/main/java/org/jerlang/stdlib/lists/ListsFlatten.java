package org.jerlang.stdlib.lists;

import org.jerlang.erts.erlang.Error;
import org.jerlang.stdlib.Lists;
import org.jerlang.type.List;
import org.jerlang.type.Term;

public class ListsFlatten {

    private ListsFlatten() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 1:
            return flatten_1(params.head().toList());
        case 2:
            List deepList = params.head().toList();
            params = params.tail();
            List tail = params.head().toList();
            return flatten_2(deepList, tail);
        default:
            throw Error.badarg;
        }
    }

    /**
     * Returns a flattened version of DeepList.
     *
     * http://www.erlang.org/doc/man/lists.html#flatten-1
     */
    public static List flatten_1(List deepList) {
        return do_flatten(deepList);
    }

    /**
     * Returns a flattened version of DeepList with the tail Tail appended.
     *
     * http://www.erlang.org/doc/man/lists.html#flatten-2
     */
    public static List flatten_2(List deepList, List tail) {
        return new List(flatten_1(deepList), tail);
    }

    private static List do_flatten(Term term) {
        if (term.isList()) {
            List list = term.toList();
            if (list.length() == 0) {
                return List.nil;
            } else {
                Term head = list.head();
                Term tail = list.tail();
                return Lists.append(do_flatten(head), do_flatten(tail));
            }
        } else {
            return new List(term);
        }
    }

}
