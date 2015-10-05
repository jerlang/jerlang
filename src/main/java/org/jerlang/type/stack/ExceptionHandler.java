package org.jerlang.type.stack;

import org.jerlang.Process;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Atom;
import org.jerlang.type.Term;

public class ExceptionHandler extends Term {

    private Term label;
    private int index;

    public ExceptionHandler(Term label, int index) {
        this.label = label;
        this.index = index;
    }

    public void handle(Process process, Error error) {
        process.setExceptionHandler(null);
        process.setX(0, Atom.of("error"));
        process.setX(1, error.reason());
    }

    public int index() {
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
