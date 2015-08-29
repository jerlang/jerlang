package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.List;
import org.jerlang.type.Map;
import org.jerlang.type.Term;

/**
 * Get a map from Source and apply (NumUpdates/2-1) updates.
 * The resulting map is stored in Destination.
 * The UpdateList contains [Key,Value] pairs.
 *
 * Arguments:
 * 1. Label
 * 2. Source
 * 3. Destination
 * 4. NumUpdates
 * 5. UpdateList
 *
 * Example:
 * {put_map_exact,{f,0},{x,0},{x,0},1,[a,2,b,3]}
 */
public class PutMapExact {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        Map map = new Map(i.arg(1).toArg(proc).toMap());
        Term destination = i.arg(2);
        List updates = i.arg(4).toList();
        while (updates.length() > 0) {
            Term key = updates.head();
            updates = updates.tail();
            Term val = updates.head().toArg(proc);
            updates = updates.tail();
            map.set(key, val);
        }
        if (destination.isXRegister()) {
            proc.setX(destination.toRegisterIndex(), map);
            return null;
        }
        throw new Error("Not implemented: " + i);
    }

}
