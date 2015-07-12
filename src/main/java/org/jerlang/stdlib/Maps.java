package org.jerlang.stdlib;

import org.jerlang.type.List;
import org.jerlang.type.Map;

/**
 * http://www.erlang.org/doc/man/maps.html
 */
public class Maps {

    public static List keys(Map map) {
        return List.nil;
    }

    public static Map map(Object fun, Map map1) {
        Map map2 = _new();
        // TODO: apply fun/1 on all elements of map1 and store in map2
        return map2;
    }

    public static Map _new() {
        return new Map();
    }

}
