package org.jerlang.exception;

import org.jerlang.type.Term;

public class ThrowException extends AbstractException {

    private final Term reason;

    public ThrowException(Term reason) {
        this.reason = reason;
    }

    public Term reason() {
        return reason;
    }

}
