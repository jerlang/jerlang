package org.jerlang.type.stack;

import org.jerlang.type.Binary;
import org.jerlang.type.Integer;
import org.jerlang.type.Term;

/**
 * See:
 * https://github.com/erlang/otp/blob/maint/erts/emulator/beam/erl_bits.h
 */
public class BinMatchState extends Term {

    private final Binary binary;
    private final int slots;
    private int offset = 0; // bit-offset

    public BinMatchState(Binary binary, int slots) {
        this.binary = binary;
        this.slots = slots;
    }

    public int tail() {
        return binary.bits() - offset;
    }

    public BinMatchState toBinMatchState() {
        return this;
    }

    @Override
    public String toString() {
        return "BinMatchState";
    }

    public Integer get_integer(int size, int unit, int flag) {
        // TODO: This assumes that unit is 1 and flag is 0.
        Integer result = new Integer(binary.extraxt_bits(offset, size));
        offset += size;
        return result;
    }

}
