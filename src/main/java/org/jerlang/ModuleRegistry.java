package org.jerlang;

import static org.jerlang.util.StringUtil.camelToSnakeCase;

import java.util.HashMap;
import java.util.Map;

import org.jerlang.erts.Erlang;
import org.jerlang.erts.Init;
import org.jerlang.erts.OtpRing0;
import org.jerlang.kernel.File;
import org.jerlang.kernel.OS;
import org.jerlang.stdlib.BeamLib;
import org.jerlang.stdlib.Binary;
import org.jerlang.stdlib.ErlInternal;
import org.jerlang.stdlib.IO;
import org.jerlang.stdlib.Lists;
import org.jerlang.stdlib.Maps;
import org.jerlang.stdlib.Shell;
import org.jerlang.type.Atom;

/**
 * Global registry for all modules.
 */
public class ModuleRegistry {

    private static final ModuleRegistry instance = new ModuleRegistry();
    private final Map<Atom, Module> modules;

    public ModuleRegistry() {
        modules = new HashMap<>();
        // TODO: Auto-register modules by reflection
        init(BeamLib.class).export();
        init(Binary.class).export();
        init(Erlang.class).export();
        init(ErlInternal.class).export();
        init(File.class).export();
        init(Init.class).export();
        init(IO.class).export();
        init(Lists.class).export();
        init(Maps.class).export();
        init(OS.class).export();
        init(OtpRing0.class).export();
        init(Shell.class).export();
    }

    public static Module get(Atom module) {
        if (!instance.modules.containsKey(module)) {
            // Maybe the module is not loaded yet
            ModuleLoader.load(module);
        }
        return instance.modules.get(module);
    }

    public static ModuleRegistry instance() {
        return instance;
    }

    public static void register(Module module) {
        instance.modules.put(module.name(), module);
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
