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

    public static Module register(String module) {
        return register(Atom.of(module));
    }

    public static Module register(Atom module) {
        instance.modules.put(module, new Module(module));
        return instance.modules.get(module);
    }

}
