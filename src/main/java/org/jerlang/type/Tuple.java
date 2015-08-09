package org.jerlang.type;

import java.util.Objects;

import org.jerlang.FunctionSignature;
import org.jerlang.erts.erlang.Error;

public class Tuple extends Term {

    private final Term[] elements;

    public Tuple(int arity) {
        elements = new Term[arity];
    }

    public int arity() {
        return elements.length;
    }

    /**
     * Get the n-th element.
     * Please note that the index starts at 1, not at 0.
     */
    public Term element(int index) {
        if (index < 1) {
            throw new Error("tuple index starts at 1, not 0");
        }
        return elements[index - 1];
    }

    @Override
    public boolean isXRegister() {
        return arity() == 2
            && element(1).equals(Atom.of("x"))
            && element(2) instanceof Integer;
    }

    @Override
    public boolean isYRegister() {
        return arity() == 2
            && element(1).equals(Atom.of("y"))
            && element(2) instanceof Integer;
    }

    public void set(int index, Term term) {
        if (index < 1) {
            throw new Error("tuple index starts at 1, not 0");
        }
        elements[index - 1] = term;
    }

    @Override
    public boolean equals(Object object) {
        if (object instanceof Tuple) {
            Tuple other = (Tuple) object;
            if (elements.length != other.elements.length) {
                return false;
            }
            for (int index = 0; index < elements.length; index++) {
                if (!elements[index].equals(other.elements[index])) {
                    return false;
                }
            }
            return true;
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Objects.hash((Object[]) elements);
    }

    @Override
    public FunctionSignature toFunctionSignature() {
        switch (arity()) {
        case 3:
            return new FunctionSignature(
                element(1).toAtom(),
                element(2).toAtom(),
                element(3).toInteger());
        case 4:
            return new FunctionSignature(
                element(1).toAtom(),
                element(2).toAtom(),
                element(3).toInteger(),
                element(4).toInteger());
        default:
            throw new Error("Cannot convert to function signature: " + this);
        }
    }

    @Override
    public Integer toRegisterIndex() {
        return element(2).toInteger();
    }

    @Override
    public String toString() {
        StringBuilder stringBuilder = new StringBuilder("{");
        for (int index = 0; index < elements.length; index++) {
            stringBuilder.append(elements[index]);
            if (index != (elements.length - 1)) {
                stringBuilder.append(',');
            }
        }
        stringBuilder.append('}');
        return stringBuilder.toString();
    }

    @Override
    public Tuple toTuple() {
        return this;
    }

    public static Tuple of(Term... terms) {
        Tuple tuple = new Tuple(terms.length);
        for (int index = 0; index < terms.length; index++) {
            tuple.set(index + 1, terms[index]);
        }
        return tuple;
    }

}
