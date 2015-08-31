package org.jerlang.type.stack;

import org.jerlang.type.Term;

public class ExceptionHandler extends Term {

    private Term label;

    public ExceptionHandler(Term label) {
        this.label = label;
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
