package org.jerlang.erts.emulator.op;

import java.math.BigInteger;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Calculate the number of bytes necessary to represent the Source in UTF8.
 *
 * Arguments:
 * 1. Label
 * 2. Source
 * 3. Destination
 *
 * Example:
 * {bs_utf8_size,{f,3},{x,0},{x,3}}
 */
public class BsUtf8Size {

    private static final BigInteger NEED1BYTE = new BigInteger("007F", 16);
    private static final BigInteger NEED2BYTES = new BigInteger("07FF", 16);
    private static final BigInteger NEED3BYTES = new BigInteger("FFFF", 16);
    private static final BigInteger NEED4BYTES = new BigInteger("1FFFFF", 16);
    private static final BigInteger NEED5BYTES = new BigInteger("3FFFFFF", 16);
    private static final BigInteger NEED6BYTES = new BigInteger("7FFFFFFF", 16);

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        Integer source = i.arg(1).toArg(proc).toInteger();
        BigInteger codepoint = source.toBigInteger();
        Term destination = i.arg(2);
        if (!destination.isXRegister()) {
            throw new Error("Not implemented: " + i);
        }
        int bytes = 0;
        if (codepoint.compareTo(NEED1BYTE) <= 0) {
            bytes = 1;
        } else if (codepoint.compareTo(NEED2BYTES) <= 0) {
            bytes = 2;
        } else if (codepoint.compareTo(NEED3BYTES) <= 0) {
            bytes = 3;
        } else if (codepoint.compareTo(NEED4BYTES) <= 0) {
            bytes = 4;
        } else if (codepoint.compareTo(NEED5BYTES) <= 0) {
            bytes = 5;
        } else if (codepoint.compareTo(NEED6BYTES) <= 0) {
            bytes = 6;
        }
        proc.setX(destination.toRegisterIndex(), Integer.of(bytes));
        return null;
    }

}
