package org.jerlang.type.stack;

import org.jerlang.Process;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Atom;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;

public class CatchExceptionHandler extends ExceptionHandler {

    public CatchExceptionHandler(Term label) {
        super(label, -1);
    }

    @Override
    public void handle(Process process, Error error) {
        process.setX(0, Tuple.of(Atom.of("EXIT"), error.reason()));
    }

    @Override
    public String toString() {
        return "{catch_exception_handler," + label() + "}";
    }

}
