package org.jerlang.type;

import java.math.BigInteger;

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

    public long toLong() {
        return value.longValue();
    }

    @Override
    public String toString() {
        return value.toString();
    }

}
