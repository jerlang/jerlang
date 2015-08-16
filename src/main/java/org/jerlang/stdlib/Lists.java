package org.jerlang.stdlib;

import org.jerlang.stdlib.lists.ListsDuplicate;
import org.jerlang.stdlib.lists.ListsReverse;
import org.jerlang.stdlib.lists.ListsSeq;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * http://www.erlang.org/doc/man/lists.html
 */
public class Lists {

    public static List duplicate(Integer n, Term elem) {
        return ListsDuplicate.duplicate_2(n, elem);
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
