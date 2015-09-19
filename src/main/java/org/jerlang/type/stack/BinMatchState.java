package org.jerlang.type.stack;

import org.jerlang.erts.erlang.Error;
import org.jerlang.type.BitString;
import org.jerlang.type.Float;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
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

    /**
     * Copy constructor
     */
    public BinMatchState(BinMatchState bms) {
        this.bitString = new BitString(bms.bitString.bytes(), bms.bitString.unusedBits());
        this.slots = bms.slots;
        this.offset = bms.offset;
    }

    public int tail() {
        return bitString.bits() - offset;
    }

    public BinMatchState toBinMatchState() {
        return this;
    }

    @Override
    public BitString toBitString() {
        return bitString;
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

        if (tail() - size < 0) {
            return null;
        }

        Integer result = new Integer(bitString.extract_bits(offset, size));
        offset += size;
        return result;
    }

    public void skip(int size, int unit, int flag) {
        offset += size * unit;
    }

    public boolean skip_utf8(int size, int flag) {
        // TODO: Implement UTF8 decoding
        if (tail() - 8 < 0) {
            return false;
        } else {
            offset += 8;
            return true;
        }
    }

    public boolean skip_utf16(int size, int flag) {
        // TODO: Implement UTF16 decoding
        if (tail() - 16 < 0) {
            return false;
        } else {
            offset += 16;
            return true;
        }
    }

    public boolean skip_utf32(int size, int flag) {
        if (tail() - 32 < 0) {
            return false;
        } else {
            offset += 32;
            return true;
        }
    }

    public Term get_utf8(int size, int flag) {
        // TODO: Implement UTF8 decoding
        if (tail() - 8 < 0) {
            return List.nil;
        } else {
            int value = bitString.extract_bits(offset, 8).intValue() & 0xFF;
            offset += 8;
            return Integer.of(value);
        }
    }

    public Term get_utf16(int size, int flag) {
        // TODO: Implement UTF16 decoding
        if (tail() - 16 < 0) {
            return List.nil;
        } else {
            int value = bitString.extract_bits(offset, 16).intValue() & 0xFFFF;
            offset += 16;
            return Integer.of(value);
        }
    }

    public Term get_utf32(int size, int flag) {
        // TODO: Implement UTF32 decoding
        if (tail() - 32 < 0) {
            return List.nil;
        } else {
            long value = bitString.extract_bits(offset, 32).longValue() & 0xFFFFFFFF;
            offset += 32;
            return new Integer(value);
        }
    }

    public boolean test_unit(int unit) {
        return (tail() % unit) == 0;
    }

    public boolean match(BitString pattern) {
        if (offset % 8 != 0) {
            throw new Error("Not implemented");
        }
        if (pattern.bits() % 8 != 0) {
            throw new Error("Not implemented");
        }
        if (tail() < pattern.bits()) {
            return false;
        }
        // Compare bytes
        int nbytes = pattern.bits() / 8;
        int boffset = offset / 8;
        for (int i = 0; i < nbytes; i++) {
            if (bitString.bytes()[boffset + i] != pattern.bytes()[i]) {
                return false;
            }
        }
        offset += (nbytes * 8);
        // Bits
        // TODO: Compare bits
        return true;
    }

    public void restore(int offset) {
        this.offset = offset;
    }

}
