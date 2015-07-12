package org.jerlang.type;

import java.util.Objects;

public class Tuple extends Term {

    private final Term[] elements;

    public Tuple(int arity) {
        elements = new Term[arity];
    }

    public int arity() {
        return elements.length;
    }

    public Term element(int index) {
        return elements[index];
    }

    public void set(int index, Term term) {
        elements[index] = term;
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
        return Objects.hashCode(elements);
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

    public static Tuple of(Term... terms) {
        Tuple tuple = new Tuple(terms.length);
        for (int index = 0; index < terms.length; index++) {
            tuple.set(index, terms[index]);
        }
        return tuple;
    }

}
