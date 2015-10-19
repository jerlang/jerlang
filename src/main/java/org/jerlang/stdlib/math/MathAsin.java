package org.jerlang.stdlib.math;

import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Float;
import org.jerlang.type.List;
import org.jerlang.type.Number;
import org.jerlang.type.Term;

public class MathAsin {

    private MathAsin() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 1:
            return asin_1(params.head().toNumber());
        default:
            throw Error.badarg;
        }
    }

    /**
     * asin
     *
     * https://en.wikipedia.org/wiki/Inverse_trigonometric_functions
     */
    public static Float asin_1(Number x) {
        return Float.of(Math.asin(x.toBigDecimal().doubleValue()));
    }

}
