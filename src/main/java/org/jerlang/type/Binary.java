package org.jerlang.type;

import java.math.BigInteger;
import java.util.Objects;

public class Binary extends Term {

    private final int[] bytes;

    public Binary(byte[] bytes) {
        this.bytes = new int[bytes.length];
        for (int index = 0; index < bytes.length; index++) {
            this.bytes[index] = bytes[index] & 0xFF;
        }
    }

    public Binary(int[] bytes) {
        this.bytes = bytes.clone();
    }

    public int bits() {
        return bytes.length * 8;
    }

    @Override
    public boolean equals(Object object) {
        if (object instanceof Binary) {
            Binary other = (Binary) object;
            if (bytes.length != other.bytes.length) {
                return false;
            }
            for (int index = 0; index < bytes.length; index++) {
                if (bytes[index] != other.bytes[index]) {
                    return false;
                }
            }
            return true;
        }
        return false;
    }

    /**
     * Extracts length bits from this binary, starting at offset.
     * The bits are converted to a BigInteger.
     * Used by BinMatchState.
     */
    public BigInteger extraxt_bits(int offset, int length) {

        if (length == 0) {
            return BigInteger.ZERO;
        }

        // Find first byte
        int byteIndex = 0;
        while (offset >= 8) {
            byteIndex++;
            offset -= 8;
        }

        int offsetMod8 = offset % 8;

        // If byte-aligned and max. 8 bits
        if (offsetMod8 == 0) {
            int b = bytes[byteIndex];
            if (length <= 8) {
                switch (length) {
                case 1:
                    return BigInteger.valueOf((b & 0b10000000) >> 7);
                case 2:
                    return BigInteger.valueOf((b & 0b11000000) >> 6);
                case 3:
                    return BigInteger.valueOf((b & 0b11100000) >> 5);
                case 4:
                    return BigInteger.valueOf((b & 0b11110000) >> 4);
                case 5:
                    return BigInteger.valueOf((b & 0b11111000) >> 3);
                case 6:
                    return BigInteger.valueOf((b & 0b11111100) >> 2);
                case 7:
                    return BigInteger.valueOf((b & 0b11111110) >> 1);
                case 8:
                    return BigInteger.valueOf(b);
                }
            }
        } else if (offsetMod8 + length <= 8) {
            int b = bytes[byteIndex];
            b <<= offsetMod8;
            switch (length) {
            case 1:
                return BigInteger.valueOf((b & 0b10000000) >> 7);
            case 2:
                return BigInteger.valueOf((b & 0b11000000) >> 6);
            case 3:
                return BigInteger.valueOf((b & 0b11100000) >> 5);
            case 4:
                return BigInteger.valueOf((b & 0b11110000) >> 4);
            case 5:
                return BigInteger.valueOf((b & 0b11111000) >> 3);
            case 6:
                return BigInteger.valueOf((b & 0b11111100) >> 2);
            case 7:
                return BigInteger.valueOf((b & 0b11111110) >> 1);
            case 8:
                return BigInteger.valueOf(b);
            }
        }

        throw new Error("Cannot extract " + length + " bits at offset " + offset);
    }

    @Override
    public int hashCode() {
        return Objects.hash(bytes);
    }

    public int length() {
        return bytes.length;
    }

    public Binary toBinary() {
        return this;
    }

    @Override
    public String toString() {
        StringBuilder stringBuilder = new StringBuilder("<<");
        for (int index = 0; index < bytes.length; index++) {
            stringBuilder.append(bytes[index]);
            if (index != bytes.length - 1) {
                stringBuilder.append(',');
            }
        }
        stringBuilder.append(">>");
        return stringBuilder.toString();
    }

    public static Binary of(int... values) {
        return new Binary(values);
    }

    public static Binary of(byte... values) {
        return new Binary(values);
    }

}
