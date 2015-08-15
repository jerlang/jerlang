package org.jerlang.stdlib.maps;

import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Boolean;
import org.jerlang.type.List;
import org.jerlang.type.Map;
import org.jerlang.type.Term;

public class MapsIsKey {

    private MapsIsKey() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 1:
            Term key = params.head();
            params = params.tail();
            Map map = params.head().toMap();
            return Boolean.of(is_key_2(key, map));
        default:
            throw new Error("badarg");
        }
    }

    /**
     * Returns `true` if map `map` contains `key` and
     * returns `false` if it does not contain the `key`.
     *
     * The call will fail with a `{badmap,Map}` exception if `map` is not a map.
     *
     * http://www.erlang.org/doc/man/maps.html#is_key-2
     */
    public static boolean is_key_2(Term key, Map map) {
        return map.is_key(key);
    }

}
