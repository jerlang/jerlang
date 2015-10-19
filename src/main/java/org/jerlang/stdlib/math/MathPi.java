package org.jerlang.stdlib.math;

import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Float;
import org.jerlang.type.List;
import org.jerlang.type.Term;

public class MathPi {

    private MathPi() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 0:
            return pi_0();
        default:
            throw Error.badarg;
        }
    }

    public static Float pi_0() {
        return Float.of(3.1415926535897932);
    }

}
