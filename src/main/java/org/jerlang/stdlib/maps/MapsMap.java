package org.jerlang.stdlib.maps;

import org.jerlang.erts.erlang.Error;
import org.jerlang.stdlib.Maps;
import org.jerlang.type.List;
import org.jerlang.type.Map;
import org.jerlang.type.Term;

public class MapsMap {

    private MapsMap() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 2:
            Term fun = params.head();
            params = params.tail();
            Map map = params.head().toMap();
            return map_2(fun, map);
        default:
            throw new Error("badarg");
        }
    }

    /**
     * http://www.erlang.org/doc/man/maps.html#map-2
     */
    public static Map map_2(Term fun, Map map1) {
        Map map2 = Maps._new();
        // TODO: apply fun/1 on all elements of map1 and store in map2
        return map2;
    }

}
