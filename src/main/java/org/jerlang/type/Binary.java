package org.jerlang.type;

import java.math.BigInteger;

/**
 * See:
 * Per Gustafsson:
 * "Programming Efficiently with Binaries and Bit Strings"
 * http://www.erlang.org/euc/07/papers/1700Gustafsson.pdf
 */
public class Binary extends BitString {

    public Binary(byte[] bytes) {
        super(bytes, 0);
    }

    public Binary(int[] bytes) {
        super(bytes, 0);
    }

    public Integer at(int pos) {
        return Integer.of(bytes[pos]);
    }

    public Binary copy(int n) {
        int bytes_len = bytes.length;
        int total_len = n * bytes_len;
        int[] new_bytes = new int[total_len];
        for (int pos = 0; pos < total_len; pos += bytes_len) {
            System.arraycopy(bytes, 0, new_bytes, pos, bytes_len);
        }
        return new Binary(new_bytes);
    }

    public Integer decode_unsigned_big() {
        byte[] b = new byte[bytes.length];
        for (int index = 0; index < bytes.length; index++) {
            b[index] = (byte) (bytes[index] & 0xFF);
        }
        return new Integer(new BigInteger(b));
    }

    public Integer decode_unsigned_little() {
        byte[] b = new byte[bytes.length];
        for (int index = 0; index < bytes.length; index++) {
            b[bytes.length - index - 1] = (byte) (bytes[index] & 0xFF);
        }
        return new Integer(new BigInteger(b));
    }

    @Override
    public boolean equals(Object object) {
        return super.equals(object);
    }

    public Binary toBinary() {
        return this;
    }

    public static Binary of(int... values) {
        return new Binary(values);
    }

    public static Binary of(byte... values) {
        return new Binary(values);
    }

    public List toList(int pos, int len) {
        // TODO: check boundaries
        List result = List.nil;
        for (int index = pos + len; index >= pos; index--) {
            result = new List(Integer.of(bytes[index]), result);
        }
        return result;
    }

}
