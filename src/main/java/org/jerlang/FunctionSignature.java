package org.jerlang;

import org.jerlang.type.Atom;
import org.jerlang.type.Integer;
import org.jerlang.type.Tuple;

/**
 * A function named f in the module m and with arity N is often denoted as m:f/N.
 */
public class FunctionSignature extends Tuple {

    public FunctionSignature(Atom module, Atom function, Integer arity) {
        super(3);
        set(0, module);
        set(1, function);
        set(2, arity);
    }

    @Override
    public String toString() {
        return new StringBuilder()
            .append(element(0))
            .append(':')
            .append(element(1))
            .append('/')
            .append(element(2))
            .toString();
    }

}
