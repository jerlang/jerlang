package org.jerlang;

import static org.jerlang.util.StringUtil.camelToSnakeCase;

import java.util.HashMap;
import java.util.Map;

import org.jerlang.erts.Erlang;
import org.jerlang.erts.Init;
import org.jerlang.erts.OtpRing0;
import org.jerlang.stdlib.BeamLib;
import org.jerlang.stdlib.ErlInternal;
import org.jerlang.stdlib.Lists;
import org.jerlang.type.Atom;

/**
 * Global registry for all modules.
 */
public class ModuleRegistry {

    private static final ModuleRegistry instance = new ModuleRegistry();
    private final Map<Atom, Module> modules;

    public ModuleRegistry() {
        modules = new HashMap<>();
        init(BeamLib.class).export();
        init(Erlang.class).export();
        init(ErlInternal.class).export();
        init(Init.class).export();
        init(Lists.class).export();
        init(OtpRing0.class).export();
    }

    public static Module get(Atom module) {
        return instance.modules.get(module);
    }

    public static ModuleRegistry instance() {
        return instance;
    }

    private Module init(Class<?> moduleClass) {
        String moduleName = camelToSnakeCase(moduleClass.getSimpleName());
        return init(moduleClass, Atom.of(moduleName));
    }

    private Module init(Class<?> moduleClass, Atom module) {
        modules.put(module, new Module(moduleClass, module));
        return modules.get(module);
    }

}
