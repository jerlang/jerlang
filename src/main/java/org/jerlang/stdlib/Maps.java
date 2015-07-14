package org.jerlang.stdlib;

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

    /**
     * http://www.erlang.org/doc/man/maps.html#get-2
     */
    public static Term get(Term key, Map map) {
        // TODO: exception {badmap, Map} if map is not a Map
        // TODO: exception {badkey, Key} if key is not in map
        return map.get(key);
    }

    /**
     * http://www.erlang.org/doc/man/maps.html#keys-1
     */
    public static List keys(Map map) {
        return List.nil;
    }

    /**
     * http://www.erlang.org/doc/man/maps.html#map-2
     */
    public static Map map(Object fun, Map map1) {
        Map map2 = _new();
        // TODO: apply fun/1 on all elements of map1 and store in map2
        return map2;
    }

    /**
     * http://www.erlang.org/doc/man/maps.html#new-0
     */
    public static Map _new() {
        return new Map();
    }

}
