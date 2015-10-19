package org.jerlang.stdlib.math;

import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Float;
import org.jerlang.type.List;
import org.jerlang.type.Number;
import org.jerlang.type.Term;

public class MathTan {

    private MathTan() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 1:
            return tan_1(params.head().toNumber());
        default:
            throw Error.badarg;
        }
    }

    /**
     * tan
     *
     * https://en.wikipedia.org/wiki/Trigonometric_functions
     */
    public static Float tan_1(Number x) {
        return Float.of(Math.tan(x.toBigDecimal().doubleValue()));
    }

}
