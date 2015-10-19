package org.jerlang.stdlib.math;

import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Float;
import org.jerlang.type.List;
import org.jerlang.type.Number;
import org.jerlang.type.Term;

public class MathCosh {

    private MathCosh() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 1:
            return cosh_1(params.head().toNumber());
        default:
            throw Error.badarg;
        }
    }

    /**
     * cosh
     *
     * https://en.wikipedia.org/wiki/Trigonometric_functions
     */
    public static Float cosh_1(Number x) {
        return Float.of(Math.cosh(x.toBigDecimal().doubleValue()));
    }

}
