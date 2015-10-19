package org.jerlang.stdlib;

import org.jerlang.erts.erlang.Error;
import org.jerlang.stdlib.math.MathAcos;
import org.jerlang.stdlib.math.MathAsin;
import org.jerlang.stdlib.math.MathAtan;
import org.jerlang.stdlib.math.MathAtan2;
import org.jerlang.stdlib.math.MathCos;
import org.jerlang.stdlib.math.MathCosh;
import org.jerlang.stdlib.math.MathExp;
import org.jerlang.stdlib.math.MathLog;
import org.jerlang.stdlib.math.MathLog10;
import org.jerlang.stdlib.math.MathPi;
import org.jerlang.stdlib.math.MathPow;
import org.jerlang.stdlib.math.MathSin;
import org.jerlang.stdlib.math.MathSinh;
import org.jerlang.stdlib.math.MathSqrt;
import org.jerlang.stdlib.math.MathTan;
import org.jerlang.stdlib.math.MathTanh;
import org.jerlang.type.Float;
import org.jerlang.type.Number;

/**
 * = math
 *
 * Mathematical Functions
 *
 * http://www.erlang.org/doc/man/math.html
 */
public class Math {

    public static Float acos(Number x) {
        return MathAcos.acos_1(x);
    }

    public static Float acosh(Number x) {
        throw new Error("not implemented");
    }

    public static Float asin(Number x) {
        return MathAsin.asin_1(x);
    }

    public static Float asinh(Number x) {
        throw new Error("not implemented");
    }

    public static Float atan(Number x) {
        return MathAtan.atan_1(x);
    }

    public static Float atan2(Number y, Number x) {
        return MathAtan2.atan2_1(y, x);
    }

    public static Float atanh(Number x) {
        throw new Error("not implemented");
    }

    public static Float cos(Number x) {
        return MathCos.cos_1(x);
    }

    public static Float cosh(Number x) {
        return MathCosh.cosh_1(x);
    }

    public static Float erf(Number x) {
        throw new Error("not implemented");
    }

    public static Float erfc(Number x) {
        throw new Error("not implemented");
    }

    public static Float exp(Number x) {
        return MathExp.exp_1(x);
    }

    public static Float log(Number x) {
        return MathLog.log_1(x);
    }

    public static Float log10(Number x) {
        return MathLog10.log10_1(x);
    }

    public static Float log2(Number x) {
        throw new Error("not implemented");
    }

    public static Float pi() {
        return MathPi.pi_0();
    }

    public static Float pow(Number x, Number y) {
        return MathPow.pow_2(x, y);
    }

    public static Float sin(Number x) {
        return MathSin.sin_1(x);
    }

    public static Float sinh(Number x) {
        return MathSinh.sinh_1(x);
    }

    public static Float sqrt(Number x) {
        return MathSqrt.sqrt_1(x);
    }

    public static Float tan(Number x) {
        return MathTan.tan_1(x);
    }

    public static Float tanh(Number x) {
        return MathTanh.tanh_1(x);
    }

}
