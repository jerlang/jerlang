package org.jerlang;

import static org.jerlang.util.StringUtil.camelToSnakeCase;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.jerlang.erts.Erlang;
import org.jerlang.erts.Erpc;
import org.jerlang.erts.Init;
import org.jerlang.erts.OtpRing0;
import org.jerlang.erts.PrimInet;
import org.jerlang.kernel.ApplicationController;
import org.jerlang.kernel.File;
import org.jerlang.kernel.GenTcp;
import org.jerlang.kernel.GenUdp;
import org.jerlang.kernel.Inet;
import org.jerlang.kernel.NetAdm;
import org.jerlang.kernel.NetKernel;
import org.jerlang.kernel.OS;
import org.jerlang.kernel.Rpc;
import org.jerlang.stdlib.Base64;
import org.jerlang.stdlib.BeamLib;
import org.jerlang.stdlib.Binary;
import org.jerlang.stdlib.Code;
import org.jerlang.stdlib.ErlInternal;
import org.jerlang.stdlib.Ets;
import org.jerlang.stdlib.IO;
import org.jerlang.stdlib.IOLib;
import org.jerlang.stdlib.Lists;
import org.jerlang.stdlib.Logger;
import org.jerlang.stdlib.Maps;
import org.jerlang.stdlib.Math;
import org.jerlang.stdlib.PersistentTerm;
import org.jerlang.stdlib.Shell;
import org.jerlang.stdlib.Timer;
import org.jerlang.stdlib.Unicode;
import org.jerlang.type.Atom;

/**
 * Global registry for all modules.
 */
public class ModuleRegistry {

    private static final ModuleRegistry instance = new ModuleRegistry();

    /**
     * Modules whose built-in implementation is only a stopgap, so a BEAM file
     * in the working directory wins. Other built-ins stay in charge: an OTP
     * erlang.beam, for example, only declares BIFs and would remove the
     * runtime's implementations of them.
     */
    private static final Set<Atom> BEAM_PREFERRED = new HashSet<>(
        Arrays.asList(Atom.of("lists"), Atom.of("timer")));

    private final Map<Atom, Module> modules;

    public ModuleRegistry() {
        modules = new HashMap<>();
        registerBuiltins();
    }

    private void registerBuiltins() {
        // TODO: Auto-register modules by reflection
        init(ApplicationController.class).export();
        init(Base64.class).export();
        init(BeamLib.class).export();
        init(Binary.class).export();
        init(Code.class).export();
        init(Erlang.class).export();
        init(Erpc.class).export();
        init(ErlInternal.class).export();
        init(Ets.class).export();
        init(File.class).export();
        init(GenTcp.class).export();
        init(GenUdp.class).export();
        init(Inet.class).export();
        init(Init.class).export();
        init(IO.class).export();
        // The Java class name IOLib does not map to io_lib by the CamelCase
        // rule, so the module name is given explicitly.
        init(IOLib.class, Atom.of("io_lib")).export();
        init(Lists.class).export();
        init(Logger.class).export();
        init(Maps.class).export();
        init(Math.class).export();
        init(NetAdm.class).export();
        init(NetKernel.class).export();
        init(OS.class).export();
        init(Rpc.class).export();
        init(OtpRing0.class).export();
        init(PersistentTerm.class).export();
        init(PrimInet.class).export();
        init(Shell.class).export();
        init(Timer.class).export();
        init(Unicode.class).export();
    }

    public static Module get(Atom module) {
        Module current = instance.modules.get(module);
        if (current != null && !current.isInternal()) {
            // Already loaded from a BEAM file; real code is never replaced.
            return current;
        }
        if (current == null || BEAM_PREFERRED.contains(module)) {
            Module loaded = ModuleLoader.load(module);
            if (loaded != null) {
                if (current != null) {
                    // Keep the built-in primitives the BEAM file declares but
                    // does not implement, such as the lists BIFs and NIFs.
                    loaded.adoptBuiltins(current);
                }
                instance.modules.put(module, loaded);
                return loaded;
            }
        }
        return current;
    }

    public static ModuleRegistry instance() {
        return instance;
    }

    /**
     * The module that is already loaded, without loading anything. This is
     * what code:which/1 and code:is_loaded/1 report on.
     */
    public static Module loaded(Atom module) {
        return instance.modules.get(module);
    }

    /**
     * Drop every loaded module and go back to the built-ins. Used by tests,
     * which load different BEAM files under the same module names.
     */
    public static void reset() {
        instance.modules.clear();
        instance.registerBuiltins();
    }

    public static void register(Module module) {
        instance.modules.put(module.name(), module);
    }

    /**
     * Forget a loaded module, so a name that was only there for one node is
     * gone afterwards. The java module is registered this way by a node that
     * asks for reflection and dropped again when that node closes.
     */
    public static void unregister(Atom module) {
        instance.modules.remove(module);
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
