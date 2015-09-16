package org.jerlang.type.stack;

import org.jerlang.type.BitString;
import org.jerlang.type.Float;
import org.jerlang.type.Integer;
import org.jerlang.type.Term;

/**
 * See:
 * https://github.com/erlang/otp/blob/maint/erts/emulator/beam/erl_bits.h
 */
public class BinMatchState extends Term {

    private final BitString bitString;
    private final int slots;
    private int offset = 0; // bit-offset

    public BinMatchState(BitString binary, int slots) {
        this.bitString = binary;
        this.slots = slots;
    }

    public int tail() {
        return bitString.bits() - offset;
    }

    public BinMatchState toBinMatchState() {
        return this;
    }

    @Override
    public String toString() {
        return "BinMatchState";
    }

    public BitString get_all_binary(int unit, int flag) {
        // TODO This assumes that unit = 1 and flag = 0
        BitString result = bitString.get_rest(offset);
        offset = bitString.bits();
        return result;
    }

    public Float get_float(int size, int unit, int flag) {
        // TODO: This assumes size = 64, unit = 1 and flag = 0.
        Integer integer = get_integer(size, unit, flag);
        return new Float(Double.longBitsToDouble(integer.toLong()));
    }

    public Integer get_integer(int size, int unit, int flag) {
        // TODO: This assumes that unit is 1 and flag is 0.
        Integer result = new Integer(bitString.extract_bits(offset, size));
        offset += size;
        return result;
    }

    public void skip(int size, int unit, int flag) {
        offset += size * unit;
    }

}
