package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.Binary;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Put Bytes bytes from string defined at Index of string table chunk.
 *
 * Arguments:
 * 1. Bytes
 * 2. Index
 */
public class BsPutString {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        Binary binary = m.beamData().stringTableChunk().strings();
        int bytes = i.arg(0).toInteger().toInt();
        int index = i.arg(1).toInteger().toInt();
        proc.bitString().put_string(binary, index, bytes);
        return null;
    }

}
