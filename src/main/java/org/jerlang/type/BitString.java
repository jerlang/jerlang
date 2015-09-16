package org.jerlang.type;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.Objects;

import org.jerlang.erts.erlang.Error;

/**
 * See:
 * Per Gustafsson:
 * "Programming Efficiently with Binaries and Bit Strings"
 * http://www.erlang.org/euc/07/papers/1700Gustafsson.pdf
 */
public class BitString extends Term {

    protected final int[] bytes;
    protected int writeOffset = 0;

    // The number of bits in the last byte
    // that are not used for this binary.
    // See BIT_BINARY_EXT external term reader.
    private final int unusedBits;

    public BitString(byte[] bytes) {
        this(bytes, 0);
    }

    public BitString(byte[] bytes, int unusedBits) {
        this.bytes = new int[bytes.length];
        this.unusedBits = unusedBits;
        for (int index = 0; index < bytes.length; index++) {
            this.bytes[index] = bytes[index] & 0xFF;
        }
    }

    public BitString(int[] bytes) {
        this(bytes, 0);
    }

    public BitString(int[] bytes, int unusedBits) {
        this.bytes = bytes.clone();
        this.unusedBits = unusedBits;
    }

    public int bits() {
        return (bytes.length * 8) - unusedBits;
    }

    public int[] bytes() {
        return bytes;
    }

    public int byte_length() {
        return bytes.length;
    }

    public List convert_to_list() {
        List list = List.nil;
        if (unusedBits == 0) {
            for (int index = bytes.length - 1; index >= 0; index--) {
                list = new List(Integer.of(bytes[index]), list);
            }
        } else {
            int[] rest = new int[1];
            rest[0] = bytes[bytes.length - 1];
            list = new List(new BitString(rest, unusedBits), list);
            for (int index = bytes.length - 2; index >= 0; index--) {
                list = new List(Integer.of(bytes[index]), list);
            }
        }
        return list;
    }

    @Override
    public boolean equals(Object object) {
        if (object instanceof BitString) {
            BitString other = (BitString) object;
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
    public BitString get_rest(int offset) {
        if (offset % 8 == 0) {
            int skipBytes = offset / 8;
            int[] newBytes = Arrays.copyOfRange(bytes, skipBytes, bytes.length);
            return new BitString(newBytes, unusedBits);
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

    public BitString toBitString() {
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

    public static BitString of(int... values) {
        return new BitString(values);
    }

    public static BitString of(byte... values) {
        return new BitString(values);
    }

    /**
     * Put the given integer into (size * unit) bits.
     */
    public void put_integer(Integer integer, Integer bits, Integer flags) {
        int num_bits = bits.toInt();

        if (num_bits == 0) {
            return;
        }

        int num = (num_bits / 8) + (num_bits % 8 == 0 ? 0 : 1);
        byte[] new_bytes = integer.toBigInteger().toByteArray();
        new_bytes = maybe_expand(new_bytes, num);
        BitString bs2 = new BitString(new_bytes, unusedBits);
        copy(bs2.bytes, 0, bytes, writeOffset, num);
        writeOffset += num;

        if (unusedBits > 0) {
            bytes[bytes.length - 1] <<= unusedBits;
            bytes[bytes.length - 1] &= 0xFF;
        }
    }

    public void put_string(Binary string, int index, int length) {
        System.arraycopy(string.bytes, index, bytes, writeOffset, length);
        writeOffset += length;
    }

    /**
     * We want to copy new_bytes to this bitstring, covering num bytes.
     * But {@link BigInteger#toByteArray()} returns sometimes smaller
     * byte arrays, so we need to expand the array before copying.
     * TODO: find a more efficient way and avoid copying the data
     */
    private byte[] maybe_expand(byte[] new_bytes, int num) {
        int srclen = new_bytes.length;
        if (srclen < num) {
            byte[] bytes = new byte[num];
            System.arraycopy(new_bytes, 0, bytes, num - srclen, srclen);
            return bytes;
        } else {
            return new_bytes;
        }
    }

    private static void copy(int[] src, int srcPos, int[] dst, int dstPos, int size) {
        System.arraycopy(src, srcPos, dst, dstPos, size);
    }

}
