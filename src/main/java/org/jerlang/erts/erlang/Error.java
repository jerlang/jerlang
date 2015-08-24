package org.jerlang.erts.erlang;

import static org.jerlang.type.List.nil;

import org.jerlang.type.List;
import org.jerlang.type.Str;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;

/**
 * Exception thrown by `erlang:error/1` and `erlang:error/2`.
 */
public class Error extends RuntimeException {

    private final Term reason;
    private final List args;

    public Error(String reason) {
        this(Str.of(reason), nil);
    }

    public Error(Term reason) {
        this(reason, nil);
    }

    public Error(Term reason, List args) {
        this.reason = reason;
        this.args = args;
    }

    public Term reason() {
        return reason;
    }

    @Override
    public String toString() {
        // TODO: Stack trace
        return Tuple.of(reason).toString();
    }

}
