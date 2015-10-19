package org.jerlang.stdlib.math;

import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Float;
import org.jerlang.type.List;
import org.jerlang.type.Number;
import org.jerlang.type.Term;

public class MathAtan2 {

    private MathAtan2() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 1:
            Number y = params.head().toNumber();
            params = params.tail();
            Number x = params.head().toNumber();
            return atan2_1(y, x);
        default:
            throw Error.badarg;
        }
    }

    /**
     * atan2
     *
     * https://en.wikipedia.org/wiki/Inverse_trigonometric_functions
     */
    public static Float atan2_1(Number y, Number x) {
        return Float.of(Math.atan2(y.toBigDecimal().doubleValue(), x.toBigDecimal().doubleValue()));
    }

}
