package org.jerlang.stdlib.math;

import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Float;
import org.jerlang.type.List;
import org.jerlang.type.Number;
import org.jerlang.type.Term;

public class MathExp {

    private MathExp() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 1:
            return exp_1(params.head().toNumber());
        default:
            throw Error.badarg;
        }
    }

    public static Float exp_1(Number x) {
        return Float.of(Math.exp(x.toBigDecimal().doubleValue()));
    }

}
