package org.jerlang.type;

public class Binary extends Term {

    private final int[] bytes;

    public Binary(byte[] bytes) {
        this.bytes = new int[bytes.length];
        for (int index = 0; index < bytes.length; index++) {
            this.bytes[index] = bytes[index] & 0xFF;
        }
    }

    public Binary(int[] bytes) {
        this.bytes = bytes;
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

}
