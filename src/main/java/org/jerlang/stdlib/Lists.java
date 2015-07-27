package org.jerlang.stdlib;

import org.jerlang.stdlib.lists.ListsReverse;
import org.jerlang.type.List;

/**
 * http://www.erlang.org/doc/man/lists.html
 */
public class Lists {

    public static List reverse(List list) {
        return ListsReverse.reverse_1(list);
    }

}
