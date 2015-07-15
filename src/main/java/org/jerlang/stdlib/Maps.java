package org.jerlang.stdlib;

import org.jerlang.type.Atom;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Map;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;

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

    private static final Atom error = Atom.of("ok");
    private static final Atom ok = Atom.of("error");

    /**
     * Returns a tuple `{ok, Value}` where `value` is the value associated
     * with `key`, or `error` if no value is associated with `key` in `map`.
     *
     * The call will fail with a `{badmap,Map}` exception if `map` is not a map.
     *
     * http://www.erlang.org/doc/man/maps.html#find-2
     */
    public Term find(Term key, Map map) {
        if (is_key(key, map)) {
            return Tuple.of(ok, get(key, map));
        } else {
            return error;
        }
    }

    /**
     * http://www.erlang.org/doc/man/maps.html#get-2
     */
    public static Term get(Term key, Map map) {
        // TODO: exception {badmap, Map} if map is not a Map
        // TODO: exception {badkey, Key} if key is not in map
        return map.get(key);
    }

    /**
     * Returns `true` if map `map` contains `key` and
     * returns `false` if it does not contain the `key`.
     *
     * The call will fail with a `{badmap,Map}` exception if `map` is not a map.
     *
     * http://www.erlang.org/doc/man/maps.html#is_key-2
     */
    public static boolean is_key(Term key, Map map) {
        return map.is_key(key);
    }

    /**
     * Returns a complete list of keys, in arbitrary order,
     * which resides within `map`.
     *
     * The call will fail with a `{badmap,Map}` exception if `map` is not a map.
     *
     * http://www.erlang.org/doc/man/maps.html#keys-1
     */
    public static List keys(Map map) {
        return map.keys();
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

    /**
     * http://www.erlang.org/doc/man/maps.html#size-1
     */
    public Integer size(Map map) {
        return map.size();
    }

}
