package org.jerlang.type;

import java.math.BigDecimal;
import java.util.Objects;

public class Float extends Number {

    private BigDecimal value;

    public Float(double value) {
        this.value = BigDecimal.valueOf(value);
    }

    public Float(BigDecimal value) {
        this.value = value;
    }

    public Float add(Float f) {
        return new Float(value.add(f.value));
    }

    public Float divide(Float f) {
        return new Float(value.divide(f.value));
    }

    @Override
    public boolean isFloat() {
        return true;
    }

    @Override
    public boolean isInteger() {
        return false;
    }

    public Float multiply(Float f) {
        return new Float(value.multiply(f.value));
    }

    public Float negate() {
        return new Float(value.negate());
    }

    public Float subtract(Float f) {
        return new Float(value.subtract(f.value));
    }

    public BigDecimal toBigDecimal() {
        return value;
    }

    public Float toFloat() {
        return this;
    }

    @Override
    public boolean equals(Object object) {
        if (object instanceof Float) {
            Float other = (Float) object;
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

    public static Float of(int value) {
        return new Float(value);
    }

    public static Float of(double value) {
        return new Float(BigDecimal.valueOf(value));
    }

    public static Float of(String value) {
        return new Float(new BigDecimal(value));
    }

}
