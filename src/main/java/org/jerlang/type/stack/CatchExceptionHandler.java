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
    public int handle(Process process, Error error) {
        process.setX(0, Tuple.of(Atom.of("EXIT"), error.reason()));
        return -1;
    }

    @Override
    public String toString() {
        return "{catch_exception_handler," + label() + "}";
    }

}
