package org.jerlang.type;

import java.math.BigDecimal;

public abstract class Number extends Term {

    public abstract boolean isFloat();

    public abstract boolean isInteger();

    public abstract BigDecimal toBigDecimal();

    @Override
    public Number toNumber() {
        return this;
    }

}
