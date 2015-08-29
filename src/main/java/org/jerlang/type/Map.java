package org.jerlang.type;

import java.util.HashMap;
import java.util.Map.Entry;

public class Map extends Term {

    private final HashMap<Term, Term> map;

    public Map() {
        map = new HashMap<>();
    }

    /**
     * Copy constructor.
     */
    public Map(Map map) {
        this.map = new HashMap<>(map.map);
    }

    @Override
    public boolean equals(Object object) {
        if (object instanceof Map) {
            Map other = (Map) object;
            return map.equals(other.map);
        }
        return false;
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

    public void set(Term key, Term value) {
        // TODO: copy on write
        map.put(key, value);
    }

    public Integer size() {
        return Integer.of(map.size());
    }

    @Override
    public Map toMap() {
        return this;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("#{");
        for (Entry<Term, Term> entry : map.entrySet()) {
            sb.append(entry.getKey());
            sb.append(" => ");
            sb.append(entry.getValue());
            sb.append(", ");
        }
        if (map.size() > 0) {
            sb.setLength(sb.length() - 2);
        }
        return sb.append("}").toString();
    }

}
