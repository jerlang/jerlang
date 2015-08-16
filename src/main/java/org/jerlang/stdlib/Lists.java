package org.jerlang.stdlib;

import org.jerlang.stdlib.lists.ListsAppend;
import org.jerlang.stdlib.lists.ListsConcat;
import org.jerlang.stdlib.lists.ListsDelete;
import org.jerlang.stdlib.lists.ListsDuplicate;
import org.jerlang.stdlib.lists.ListsFlatten;
import org.jerlang.stdlib.lists.ListsReverse;
import org.jerlang.stdlib.lists.ListsSeq;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * http://www.erlang.org/doc/man/lists.html
 */
public class Lists {

    public static List append(List listOfLists) {
        return ListsAppend.append_1(listOfLists);
    }

    public static List append(List list1, List list2) {
        return ListsAppend.append_2(list1, list2);
    }

    public static List concat(List things) {
        return ListsConcat.concat_1(things);
    }

    public static List delete(Term element, List list) {
        return ListsDelete.delete_2(element, list);
    }

    public static List duplicate(Integer n, Term elem) {
        return ListsDuplicate.duplicate_2(n, elem);
    }

    public static List flatten(List deepList) {
        return ListsFlatten.flatten_1(deepList);
    }

    public static List flatten(List deepList, List tail) {
        return ListsFlatten.flatten_2(deepList, tail);
    }

    public static List reverse(List list) {
        return ListsReverse.reverse_1(list);
    }

    public static List seq(Integer from, Integer to) {
        return ListsSeq.seq_2(from, to);
    }

    public static List seq(Integer from, Integer to, Integer incr) {
        return ListsSeq.seq_3(from, to, incr);
    }

}
