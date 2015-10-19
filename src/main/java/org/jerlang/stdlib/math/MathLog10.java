package org.jerlang.stdlib.math;

import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Float;
import org.jerlang.type.List;
import org.jerlang.type.Number;
import org.jerlang.type.Term;

public class MathLog10 {

    private MathLog10() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 1:
            return log10_1(params.head().toNumber());
        default:
            throw Error.badarg;
        }
    }

    public static Float log10_1(Number x) {
        return Float.of(Math.log10(x.toBigDecimal().doubleValue()));
    }

}
