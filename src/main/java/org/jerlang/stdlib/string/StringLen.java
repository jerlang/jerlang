package org.jerlang.stdlib.string;

import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Atom;
import org.jerlang.type.List;
import org.jerlang.type.Map;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;

public class StringLen {

    private StringLen() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 2:
            Term key = params.head();
            params = params.tail();
            Map map = params.head().toMap();
            return find_2(key, map);
        default:
            throw new Error("badarg");
        }
    }

    /**
     * Returns a tuple {ok, Value} where Value is the value associated
     * with Key, or error if no value is associated with Key in Map.
     *
     * The call will fail with a {badmap,Map} exception if Map is not a map.
     *
     * http://www.erlang.org/doc/man/maps.html#find-2
     */
    public static Term find_2(Term key, Map map) {
        Term result = map.get(key);
        if (result != null) {
            return Tuple.of(Atom.of("ok"), result);
        } else {
            return Atom.of("error");
        }
    }

}
