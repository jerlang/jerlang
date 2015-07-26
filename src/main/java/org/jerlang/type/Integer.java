package org.jerlang.type;

import java.math.BigInteger;
import java.util.Objects;

public class Integer extends Number {

    private BigInteger value;

    public Integer(long value) {
        this.value = BigInteger.valueOf(value);
    }

    public Integer(BigInteger value) {
        this.value = value;
    }

    @Override
    public boolean isFloat() {
        return false;
    }

    @Override
    public boolean isInteger() {
        return true;
    }

    public BigInteger toBigInteger() {
        return value;
    }

    public int toInt() {
        return value.intValue();
    }

    public Integer toInteger() {
        return this;
    }

    public long toLong() {
        return value.longValue();
    }

    @Override
    public boolean equals(Object object) {
        if (object instanceof Integer) {
            Integer other = (Integer) object;
            return value.equals(other.value);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Objects.hash(value);
    }

    @Override
    public String toString() {
        return value.toString();
    }

    public static Integer of(int value) {
        return new Integer(value);
    }

    public static Integer of(String value) {
        return new Integer(Long.parseLong(value));
    }

}
