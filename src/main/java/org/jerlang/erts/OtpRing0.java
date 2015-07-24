package org.jerlang.erts;

import org.jerlang.ModuleRegistry;
import org.jerlang.type.Atom;
import org.jerlang.type.Integer;
import org.jerlang.type.Str;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;

/**
 * Start up of Erlang system.
 *
 * https://github.com/erlang/otp/blob/master/erts/preloaded/src/otp_ring0.erl
 */
public class OtpRing0 {

    static {
        ModuleRegistry.register("otp_ring0")
            .export("start", 2);
    }

    public static Term start(Term _env, Term argv) {
        return run(Atom.of("init"), Atom.of("boot"), argv);
    }

    private static Term run(Term m, Term f, Term a) {
        if (Erlang.function_exported(m.toAtom(), f.toAtom(), Integer.of(1))) {
            return Erlang.apply(m, f, a);
        } else {
            Erlang.display(Tuple.of(
                Atom.of("fatal"),
                Atom.of("error"),
                Atom.of("module"),
                m,
                Str.of("does not export"),
                f,
                Str.of("/1")
                ));
            Erlang.halt(Integer.of(1));
            return null;
        }
    }

}
