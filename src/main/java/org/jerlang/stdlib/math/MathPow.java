package org.jerlang.stdlib.math;

import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Float;
import org.jerlang.type.List;
import org.jerlang.type.Number;
import org.jerlang.type.Term;

public class MathPow {

    private MathPow() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 2:
            Number x = params.head().toNumber();
            params = params.tail();
            Number y = params.head().toNumber();
            return pow_2(x, y);
        default:
            throw Error.badarg;
        }
    }

    public static Float pow_2(Number x, Number y) {
        return Float.of(Math.pow(x.toBigDecimal().doubleValue(), y.toBigDecimal().doubleValue()));
    }

}
