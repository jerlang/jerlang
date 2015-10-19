package org.jerlang.stdlib.math;

import org.apache.commons.math3.special.Erf;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Float;
import org.jerlang.type.List;
import org.jerlang.type.Number;
import org.jerlang.type.Term;

public class MathErf {

    private MathErf() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 1:
            return erf_1(params.head().toNumber());
        default:
            throw Error.badarg;
        }
    }

    /**
     * https://en.wikipedia.org/wiki/Error_function
     */
    public static Float erf_1(Number x) {
        return Float.of(Erf.erf(x.toBigDecimal().doubleValue()));
    }

}
