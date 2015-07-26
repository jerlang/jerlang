package org.jerlang;

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
        init(BeamLib.class, "beam_lib").export(BeamLib.EXPORT);
        init(Erlang.class, "erlang").export(Erlang.EXPORT);
        init(ErlInternal.class, "erl_internal").export(ErlInternal.EXPORT);
        init(Init.class, "init").export(Init.EXPORT);
        init(Lists.class, "lists").export(Lists.EXPORT);
        init(OtpRing0.class, "otp_ring_0").export(OtpRing0.EXPORT);
    }

    public static Module get(Atom module) {
        return instance.modules.get(module);
    }

    public static ModuleRegistry instance() {
        return instance;
    }

    private Module init(Class<?> moduleClass, String module) {
        return init(moduleClass, Atom.of(module));
    }

    private Module init(Class<?> moduleClass, Atom module) {
        modules.put(module, new Module(moduleClass, module));
        return modules.get(module);
    }

}
