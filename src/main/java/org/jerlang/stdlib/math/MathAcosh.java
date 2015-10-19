package org.jerlang.stdlib.math;

import org.apache.commons.math3.util.FastMath;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Float;
import org.jerlang.type.List;
import org.jerlang.type.Number;
import org.jerlang.type.Term;

public class MathAcosh {

    private MathAcosh() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 1:
            return acosh_1(params.head().toNumber());
        default:
            throw Error.badarg;
        }
    }

    public static Float acosh_1(Number x) {
        return Float.of(FastMath.acosh(x.toBigDecimal().doubleValue()));
    }

}
