package org.jerlang.stdlib.math;

import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Float;
import org.jerlang.type.List;
import org.jerlang.type.Number;
import org.jerlang.type.Term;

public class MathLog {

    private MathLog() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 1:
            return log_1(params.head().toNumber());
        default:
            throw Error.badarg;
        }
    }

    public static Float log_1(Number x) {
        return Float.of(Math.log(x.toBigDecimal().doubleValue()));
    }

}
