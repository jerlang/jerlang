package org.jerlang.erts.otp_ring0;

import org.jerlang.erts.Erlang;
import org.jerlang.erts.erlang.ErlangDisplay;
import org.jerlang.erts.erlang.Error;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.Atom;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Str;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;

public class OtpRing0Start {

    public static Term dispatch(List params) throws ThrowException {
        switch (params.length()) {
        case 2:
            Term env = params.head();
            params = params.tail();
            Term argv = params.head();
            return start_2(env, argv);
        default:
            throw new Error("badarg");
        }
    }

    public static Term start_2(Term _env, Term argv) throws ThrowException {
        return run(Atom.of("init"), Atom.of("boot"), argv);
    }

    private static Term run(Term m, Term f, Term a) throws ThrowException {
        if (Erlang.function_exported(m.toAtom(), f.toAtom(), Integer.ONE).isTrue()) {
            return Erlang.apply(m, f, a);
        } else {
            ErlangDisplay.display_1(Tuple.of(
                Atom.of("fatal"),
                Atom.of("error"),
                Atom.of("module"),
                m,
                Str.of("does not export"),
                f,
                Str.of("/1")));
            Erlang.halt(Integer.ONE);
            return null;
        }
    }

}
