package org.jerlang.stdlib.lists;

import org.jerlang.erts.erlang.Error;
import org.jerlang.stdlib.Lists;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Returns a sequence of integers which starts with From and
 * contains the successive results of adding Incr to the previous element,
 * until To has been reached or passed
 * (in the latter case, To is not an element of the sequence).
 * Incr defaults to 1.
 *
 * Failure: If To<From-Incr and Incr is positive,
 * or if To>From-Incr and Incr is negative,
 * or if Incr==0 and From/=To.
 */
public class ListsSeq {

    private ListsSeq() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 2: {
            Integer from = params.head().toInteger();
            params = params.tail();
            Integer to = params.head().toInteger();
            return seq_2(from, to);
        }
        case 3: {
            Integer from = params.head().toInteger();
            params = params.tail();
            Integer to = params.head().toInteger();
            params = params.tail();
            Integer incr = params.head().toInteger();
            return seq_3(from, to, incr);
        }
        default:
            throw new Error("badarg");
        }
    }

    /**
     * See:
     * http://www.erlang.org/doc/man/lists.html#seq-2
     */
    public static List seq_2(Integer from, Integer to) {
        return seq_3(from, to, Integer.ONE);
    }

    /**
     * See:
     * http://www.erlang.org/doc/man/lists.html#seq-3
     */
    public static List seq_3(Integer from, Integer to, Integer incr) {
        List result = List.nil;
        for (int i = from.toInt(); i <= to.toInt(); i += incr.toInt()) {
            result = new List(Integer.of(i), result);
        }
        return Lists.reverse(result);
    }

}
