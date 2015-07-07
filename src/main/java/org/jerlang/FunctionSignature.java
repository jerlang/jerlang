package org.jerlang;

import org.jerlang.type.Atom;
import org.jerlang.type.Integer;

/**
 * A function named f in the module m and with arity N is often denoted as m:f/N.
 */
public class FunctionSignature {

    private final Atom module;
    private final Atom function;
    private final Integer arity;

    public FunctionSignature(Atom module, Atom function, Integer arity) {
        this.module = module;
        this.function = function;
        this.arity = arity;
    }

    @Override
    public boolean equals(Object object) {
        if (object instanceof FunctionSignature) {
            FunctionSignature other = (FunctionSignature) object;
            return module.equals(other.module)
                && function.equals(other.function)
                && arity.equals(other.arity);
        }
        return false;
    }

    @Override
    public String toString() {
        return new StringBuilder()
            .append(module)
            .append(':')
            .append(function)
            .append('/')
            .append(arity).toString();
    }

}
