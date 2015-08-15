package org.jerlang.erts;

import org.jerlang.erts.otp_ring0.OtpRing0Start;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.Term;

/**
 * Start up of Erlang system.
 *
 * https://github.com/erlang/otp/blob/master/erts/preloaded/src/otp_ring0.erl
 */
public class OtpRing0 {

    public static Term start(Term env, Term argv) throws ThrowException {
        return OtpRing0Start.start_2(env, argv);
    }

}
