package org.jerlang;

import org.jerlang.stdlib.BeamLib;
import org.jerlang.stdlib.beam_lib.BeamData;
import org.jerlang.type.Atom;
import org.jerlang.type.List;
import org.jerlang.type.Str;
import org.jerlang.type.Term;

/**
 * The ModuleLoader loads a BEAM file into a BeamData object and
 * registers the loaded module at ModuleRegistry.
 */
public class ModuleLoader {

    /**
     * Try to load the module from file system
     */
    public static void load(Atom module) {
        String filename = module.toString() + ".beam";
        Term info = BeamLib.info(Str.of(filename));
        if (!(info instanceof List)) {
            return;
        }
        Module m = new Module(loadBeamData(filename, info.toList()), module);
        ModuleRegistry.register(m);
    }

    private static BeamData loadBeamData(String filename, List info) {
        // TODO: parse chunks
        return new BeamData(null, null, null, null, null, null, null, null, null);
    }

}
