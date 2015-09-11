package org.jerlang.type;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.Objects;

import org.jerlang.erts.erlang.Error;

public class Binary extends Term {

    private final int[] bytes;

    // The number of bits in the last byte
    // that are not used for this binary.
    // See BIT_BINARY_EXT external term reader.
    private final int unusedBits;

    public Binary(byte[] bytes) {
        this(bytes, 0);
    }

    public Binary(byte[] bytes, int unusedBits) {
        this.bytes = new int[bytes.length];
        this.unusedBits = unusedBits;
        for (int index = 0; index < bytes.length; index++) {
            this.bytes[index] = bytes[index] & 0xFF;
        }
    }

    public Binary(int[] bytes) {
        this(bytes, 0);
    }

    public Binary(int[] bytes, int unusedBits) {
        this.bytes = bytes.clone();
        this.unusedBits = unusedBits;
    }

    public Integer at(int pos) {
        return Integer.of(bytes[pos]);
    }

    public int bits() {
        return (bytes.length * 8) - unusedBits;
    }

    public int byte_length() {
        return bytes.length;
    }

    @Override
    public boolean equals(Object object) {
        if (object instanceof Binary) {
            Binary other = (Binary) object;
            if (bytes.length != other.bytes.length) {
                return false;
            }
            if (unusedBits != other.unusedBits) {
                return false;
            }
            for (int index = 0; index < bytes.length; index++) {
                if (index == (bytes.length - 1) && (unusedBits > 0)) {
                    int a = (bytes[index] >> unusedBits) & 0xFF;
                    int b = (other.bytes[index] >> unusedBits) & 0xFF;
                    if (a != b) {
                        return false;
                    }
                } else {
                    if (bytes[index] != other.bytes[index]) {
                        return false;
                    }
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
    public BigInteger extract_bits(int offset, int length) {

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
            } else if (length % 8 == 0) {
                // byte-aligned and n-byte-length
                int n = length / 8;
                BigInteger result = BigInteger.ZERO;
                while (n-- > 0) {
                    int bv = bytes[byteIndex++] & 0xFF;
                    result = result.shiftLeft(8);
                    result = result.add(BigInteger.valueOf(bv));
                }
                return result;
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

    /**
     * Return a binary with all remaining bits
     */
    public Binary get_rest(int offset) {
        if (offset % 8 == 0) {
            int skipBytes = offset / 8;
            int[] newBytes = Arrays.copyOfRange(bytes, skipBytes, bytes.length);
            return new Binary(newBytes, unusedBits);
        } else {
            throw new Error("Not supported offset: " + offset);
        }
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
            if (index == (bytes.length - 1)) {
                if (unusedBits == 0) {
                    stringBuilder.append(bytes[index]);
                } else {
                    int byteValue = (bytes[index] >> unusedBits) & 0xFF;
                    stringBuilder.append(byteValue);
                    stringBuilder.append(':');
                    stringBuilder.append(8 - unusedBits);
                }
            } else {
                stringBuilder.append(bytes[index]);
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
