package org.jerlang.type.stack;

import org.jerlang.type.Atom;
import org.jerlang.type.Term;

public class ExceptionHandler extends Term {

    private Term label;
    private int index;

    public ExceptionHandler(Term label, int index) {
        this.label = label;
        this.index = index;
    }

    public int handle(org.jerlang.Process process, org.jerlang.erts.erlang.Error error) {
        process.setExceptionHandler(null);
        process.setX(0, Atom.of("error"));
        process.setX(1, error.reason());
        return index;
    }

    public Term label() {
        return label;
    }

    public ExceptionHandler toExceptionHandler() {
        return this;
    }

    @Override
    public String toString() {
        return "{exception_handler," + label + "}";
    }

}
