package org.jerlang;

import java.util.Objects;

import org.jerlang.type.Atom;
import org.jerlang.type.Integer;
import org.jerlang.type.Tuple;

/**
 * A function named f in the module m and with arity N is often denoted as m:f/N.
 */
public class FunctionSignature extends Tuple {

    public FunctionSignature(String module, String function, int arity) {
        this(Atom.of(module), Atom.of(function), Integer.of(arity));
    }

    public FunctionSignature(Module module, String function, int arity) {
        this(module.name(), Atom.of(function), Integer.of(arity));
    }

    public FunctionSignature(Atom module, Atom function, Integer arity) {
        super(3);
        set(0, module);
        set(1, function);
        set(2, arity);
    }

    public FunctionSignature(Atom module, Atom function, Integer arity, Integer label) {
        super(4);
        set(0, module);
        set(1, function);
        set(2, arity);
        set(3, label);
    }

    @Override
    public boolean equals(Object object) {
        if (object instanceof Tuple) {
            Tuple other = (Tuple) object;
            return other.arity() >= 3
                && element(1).equals(other.element(1))
                && element(2).equals(other.element(2))
                && element(3).equals(other.element(3));
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Objects.hash(element(1), element(2), element(3));
    }

    @Override
    public String toString() {
        String string = "" + element(1) + ":" + element(2) + "/" + element(3);
        if (arity() == 4) {
            string += " (label " + element(3) + ")";
        }
        return string;
    }

}
