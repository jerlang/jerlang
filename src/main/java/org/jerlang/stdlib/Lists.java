package org.jerlang.stdlib;

import org.jerlang.erts.Erlang;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * http://www.erlang.org/doc/man/lists.html
 */
public class Lists {

    public static final String[] EXPORT = {
        "reverse/1"
    };

    public static Term reverse(List params) {
        switch (Erlang.length_1(params).toInt()) {
        case 1:
            return reverse_1(params.head().toList());
        default:
            throw new Error("badarg");
        }
    }

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
