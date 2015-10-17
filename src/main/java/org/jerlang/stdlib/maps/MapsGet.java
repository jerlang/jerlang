package org.jerlang.stdlib.maps;

import org.jerlang.erts.erlang.Error;
import org.jerlang.type.List;
import org.jerlang.type.Map;
import org.jerlang.type.Term;

public class MapsGet {

    private MapsGet() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 1:
            Term key = params.head();
            params = params.tail();
            Map map = params.head().toMap();
            return get_2(key, map);
        default:
            throw Error.badarg;
        }
    }

    /**
     * http://www.erlang.org/doc/man/maps.html#get-2
     */
    public static Term get_2(Term key, Map map) {
        // TODO: exception {badmap, Map} if map is not a Map
        // TODO: exception {badkey, Key} if key is not in map
        return map.get(key);
    }

}
