package org.jerlang.stdlib.math;

import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Float;
import org.jerlang.type.List;
import org.jerlang.type.Number;
import org.jerlang.type.Term;

public class MathCos {

    private MathCos() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 1:
            return cos_1(params.head().toNumber());
        default:
            throw Error.badarg;
        }
    }

    /**
     * cos
     *
     * https://en.wikipedia.org/wiki/Trigonometric_functions
     */
    public static Float cos_1(Number x) {
        return Float.of(Math.cos(x.toBigDecimal().doubleValue()));
    }

}
