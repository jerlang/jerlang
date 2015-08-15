package org.jerlang.stdlib;

import org.jerlang.stdlib.maps.MapsFind;
import org.jerlang.stdlib.maps.MapsGet;
import org.jerlang.stdlib.maps.MapsIsKey;
import org.jerlang.stdlib.maps.MapsKeys;
import org.jerlang.stdlib.maps.MapsMap;
import org.jerlang.stdlib.maps.MapsNew;
import org.jerlang.stdlib.maps.MapsSize;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Map;
import org.jerlang.type.Term;

/**
 * = maps
 *
 * == Summary
 *
 * Maps Processing Functions
 *
 * == Description
 *
 * This module contains functions for maps processing.
 *
 * Based on:
 * http://www.erlang.org/doc/man/maps.html
 */
public class Maps {

    public Term find(Term key, Map map) {
        return MapsFind.find_2(key, map);
    }

    /**
     * http://www.erlang.org/doc/man/maps.html#get-2
     */
    public static Term get(Term key, Map map) {
        return MapsGet.get_2(key, map);
    }

    public static boolean is_key(Term key, Map map) {
        return MapsIsKey.is_key_2(key, map);
    }

    public static List keys(Map map) {
        return MapsKeys.keys_1(map);
    }

    public static Map map(Term fun, Map map1) {
        return MapsMap.map_2(fun, map1);
    }

    public static Map _new() {
        return MapsNew.new_0();
    }

    public Integer size(Map map) {
        return MapsSize.size_1(map);
    }

}
