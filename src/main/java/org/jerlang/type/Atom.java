package org.jerlang.type;

/**
 * An atom is a literal, a constant with name.
 * 
 * An atom is to be enclosed in single quotes (')
 * if it does not begin with a lower-case letter or
 * if it contains other characters than alphanumeric
 * characters, underscore (_), or @.
 * 
 * http://www.erlang.org/doc/reference_manual/data_types.html
 */
public class Atom extends Term {

    private String name;

    public Atom(String name) {
        this.name = name;
    }

    @Override
    public boolean equals(Object object) {
        return name.equals(object);
    }

    @Override
    public int hashCode() {
        return name.hashCode();
    }

    @Override
    public String toString() {
        return name;
    }

}
