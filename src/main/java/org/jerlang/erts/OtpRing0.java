package org.jerlang.erts;

import org.jerlang.erts.erlang.ErlangApply;
import org.jerlang.erts.erlang.ErlangDisplay;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Atom;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Str;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;

/**
 * Start up of Erlang system.
 *
 * https://github.com/erlang/otp/blob/master/erts/preloaded/src/otp_ring0.erl
 */
public class OtpRing0 {

    public static final String[] EXPORT = {
        "start/2",
    };

    public static Term start(List params) {
        switch (Erlang.length_1(params).toInt()) {
        case 2:
            Term env = params.head();
            params = params.tail();
            Term argv = params.head();
            return start_2(env, argv);
        default:
            throw new Error("badarg");
        }
    }

    static Term start_2(Term _env, Term argv) {
        return run(Atom.of("init"), Atom.of("boot"), argv);
    }

    private static Term run(Term m, Term f, Term a) {
        if (Erlang.function_exported_3(m.toAtom(), f.toAtom(), Integer.of(1))) {
            return ErlangApply.apply_3(m, f, a);
        } else {
            ErlangDisplay.display_1(Tuple.of(
                Atom.of("fatal"),
                Atom.of("error"),
                Atom.of("module"),
                m,
                Str.of("does not export"),
                f,
                Str.of("/1")
                ));
            Erlang.halt_1(Integer.of(1));
            return null;
        }
    }

}
