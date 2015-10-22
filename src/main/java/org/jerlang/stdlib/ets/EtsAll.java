package org.jerlang.stdlib.ets;

import org.jerlang.erts.erlang.Error;
import org.jerlang.type.List;
import org.jerlang.type.Term;

public class EtsAll {

    private EtsAll() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 0:
            return all_0();
        default:
            throw Error.badarg;
        }
    }

    /**
     * Returns a list of all tables at the node.
     * Named tables are given by their names,
     * unnamed tables are given by their table identifiers.
     *
     * There is no guarantee of consistency in the returned list.
     * Tables created or deleted by other processes "during" the
     * ets:all() call may or may not be included in the list.
     * Only tables created/deleted before ets:all() is called are
     * guaranteed to be included/excluded.
     *
     * http://www.erlang.org/doc/man/ets.html#all-0
     */
    public static Term all_0() {
        List result = List.nil;
        for (Table table : TableRegistry.tables()) {
            if (table.named_table()) {
                result = new List(table.name(), result);
            } else {
                result = new List(table.id(), result);
            }
        }
        return result;
    }

}
