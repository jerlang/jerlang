package org.jerlang.type;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Objects;

public class Integer extends Number {

    public static final Integer ZERO = Integer.of(0);
    public static final Integer ONE = Integer.of(1);

    private BigInteger value;
    private BigDecimal bigDecimal = null;

    public Integer(long value) {
        this.value = BigInteger.valueOf(value);
    }

    public Integer(BigInteger value) {
        this.value = value;
    }

    public Integer add(Integer other) {
        return new Integer(value.add(other.value));
    }

    @Override
    public boolean isFloat() {
        return false;
    }

    @Override
    public boolean isInteger() {
        return true;
    }

    public Integer multiply(Integer other) {
        return new Integer(value.multiply(other.value));
    }

    @Override
    public BigDecimal toBigDecimal() {
        if (bigDecimal == null) {
            bigDecimal = new BigDecimal(value);
        }
        return bigDecimal;
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
