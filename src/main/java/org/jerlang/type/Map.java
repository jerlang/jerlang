package org.jerlang.type;

import java.util.HashMap;

public class Map extends Term {

    private final HashMap<Term, Term> map;

    public Map() {
        map = new HashMap<>();
    }

}