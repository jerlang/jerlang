package org.jerlang.stdlib.maps;

import org.jerlang.erts.erlang.Error;
import org.jerlang.type.List;
import org.jerlang.type.Map;
import org.jerlang.type.Term;

public class MapsKeys {

    private MapsKeys() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 1:
            Map map = params.head().toMap();
            return keys_1(map);
        default:
            throw new Error("badarg");
        }
    }

    /**
     * Returns a complete list of keys, in arbitrary order,
     * which resides within `map`.
     *
     * The call will fail with a `{badmap,Map}` exception if `map` is not a map.
     *
     * http://www.erlang.org/doc/man/maps.html#keys-1
     */
    public static List keys_1(Map map) {
        return map.keys();
    }

}
