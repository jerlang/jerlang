package org.jerlang;

import java.util.HashMap;
import java.util.Map;

import org.jerlang.type.Atom;

/**
 * Global registry for all modules.
 */
public class ModuleRegistry {

    private static final ModuleRegistry instance = new ModuleRegistry();
    private final Map<Atom, Module> modules;

    public ModuleRegistry() {
        modules = new HashMap<>();
    }

    public Module get(Atom module) {
        return modules.get(module);
    }

    public Module get(Object module) {
        return null;
    }

    public static ModuleRegistry instance() {
        return instance;
    }

}
