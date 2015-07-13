package org.jerlang;

import java.util.HashMap;
import java.util.Map;

import org.jerlang.type.Atom;

public class AtomRegistry {

    private static final AtomRegistry instance = new AtomRegistry();
    private final Map<String, Atom> atoms;

    public AtomRegistry() {
        atoms = new HashMap<>();
    }

    public Atom get(String name) {
        return atoms.get(name);
    }

    public static AtomRegistry instance() {
        return instance;
    }

    public static void register(Atom atom) {
        instance.atoms.put(atom.toString(), atom);
    }

}
