package org.jerlang.type;

import java.util.HashMap;

public class Map extends Term {

    private final HashMap<Term, Term> map;

    public Map() {
        map = new HashMap<>();
    }

    public boolean is_key(Term key) {
        return map.containsKey(key);
    }

    public Term get(Term key) {
        return map.get(key);
    }

    public List keys() {
        return List.of(map.keySet());
    }

    public Integer size() {
        return Integer.of(map.size());
    }

}
