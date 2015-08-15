package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.List;
import org.jerlang.type.Map;
import org.jerlang.type.Term;

/**
 * Verify that map in Source has all keys in FieldList.
 * If not, jump to Label.
 *
 * Arguments:
 * 1. Label
 * 2. Source
 * 3. FieldList
 *
 * Example:
 * {has_map_fields,{f,4},{x,0},[b]}
 */
public class HasMapFields {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        Map map = i.arg(1).toArg(proc).toMap();
        List fields = i.arg(2).toList();
        while (fields.length() > 0) {
            if (!map.is_key(fields.head())) {
                return i.arg(0);
            }
            fields = fields.tail();
        }
        return null;
    }

}
