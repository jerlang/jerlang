package org.jerlang.type;

import java.util.ArrayList;

public class Tuple extends Term {

    private final ArrayList<Term> elements;

    public Tuple(int arity) {
        elements = new ArrayList<>(arity);
    }

    public Term element(int index) {
        return elements.get(index);
    }

    public void set(int index, Term term) {
        elements.set(index, term);
    }

    public static Tuple of(Term... terms) {
        Tuple tuple = new Tuple(terms.length);
        for (int index = 0; index < terms.length; index++) {
            tuple.set(index, terms[index]);
        }
        return tuple;
    }

}
