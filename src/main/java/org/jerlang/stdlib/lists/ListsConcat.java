package org.jerlang.stdlib.lists;

import org.jerlang.erts.erlang.Error;
import org.jerlang.type.List;
import org.jerlang.type.Str;
import org.jerlang.type.Term;

public class ListsConcat {

    private ListsConcat() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 1:
            return concat_1(params.head().toList());
        default:
            throw new Error("badarg");
        }
    }

    /**
     * Concatenates the text representation of the elements of Things.
     * The elements of Things can be atoms, integers, floats or strings.
     *
     * http://www.erlang.org/doc/man/lists.html#concat-1
     */
    public static List concat_1(List things) {
        StringBuilder stringBuilder = new StringBuilder();
        while (things.length() > 0) {
            stringBuilder.append(things.head());
            things = things.tail();
        }
        return Str.of(stringBuilder.toString());
    }

}
