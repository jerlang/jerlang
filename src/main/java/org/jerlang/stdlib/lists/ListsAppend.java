package org.jerlang.stdlib.lists;

import org.jerlang.erts.erlang.Error;
import org.jerlang.stdlib.Lists;
import org.jerlang.type.List;
import org.jerlang.type.Term;

public class ListsAppend {

    private ListsAppend() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 1:
            return append_1(params.head().toList());
        case 2:
            List list1 = params.head().toList();
            params = params.tail();
            List list2 = params.head().toList();
            return append_2(list1, list2);
        default:
            throw new Error("badarg");
        }
    }

    /**
     * Returns a list in which all the sub-lists of ListOfLists have been appended.
     *
     * http://www.erlang.org/doc/man/lists.html#append-1
     */
    public static List append_1(List listOfLists) {
        List result = List.nil;
        while (listOfLists.length() > 0) {
            List subList = listOfLists.head().toList();
            while (subList.length() > 0) {
                result = new List(subList.head(), result);
                subList = subList.tail();
            }
            listOfLists = listOfLists.tail();
        }
        return Lists.reverse(result);
    }

    /**
     * Returns a new list List3 which is made from the elements of List1
     * followed by the elements of List2.
     *
     * http://www.erlang.org/doc/man/lists.html#append-2
     */
    public static List append_2(List list1, List list2) {
        List result = List.nil;
        while (list1.length() > 0) {
            result = new List(list1.head(), result);
            list1 = list1.tail();
        }
        while (list2.length() > 0) {
            result = new List(list2.head(), result);
            list2 = list2.tail();
        }
        return Lists.reverse(result);
    }

}
