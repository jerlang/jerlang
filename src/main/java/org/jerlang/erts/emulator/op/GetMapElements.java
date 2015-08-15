package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.List;
import org.jerlang.type.Map;
import org.jerlang.type.Term;

/**
 * Arguments:
 * 1. Label
 * 2. Source
 * 3. List
 *
 * Example:
 * {get_map_elements,{f,4},{x,0},[a,{x,1}]}
 */
public class GetMapElements {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        Map map = i.arg(1).toArg(proc).toMap();
        List list = i.arg(2).toList();
        if (list.length() % 2 != 0) {
            throw new Error("list size is not even");
        }
        while (list.length() > 0) {
            Term key = list.head();
            list = list.tail();
            Term dst = list.head();
            list = list.tail();
            Term val = map.get(key);
            if (dst.isXRegister()) {
                proc.setX(dst.toRegisterIndex(), val);
            } else {
                throw new Error("Not implemented dst: " + i);
            }
        }
        return null;
    }

}
