package org.jerlang.stdlib.maps;

import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Map;
import org.jerlang.type.Term;

public class MapsSize {

    private MapsSize() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 1:
            Map map = params.head().toMap();
            return size_1(map);
        default:
            throw Error.badarg;
        }
    }

    /**
     * http://www.erlang.org/doc/man/maps.html#size-1
     */
    public static Integer size_1(Map map) {
        return map.size();
    }

}
