package org.jerlang.type;

import static java.nio.charset.StandardCharsets.ISO_8859_1;

import org.jerlang.AtomRegistry;

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

    private Atom(String name) {
        this.name = name;
    }

    @Override
    public boolean equals(Object object) {
        if (object instanceof Atom) {
            Atom other = (Atom) object;
            return name.equals(other.name);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return name.hashCode();
    }

    @Override
    public String toString() {
        return name;
    }

    public static Atom of(byte[] bytes) {
        return of(new String(bytes, ISO_8859_1));
    }

    public static Atom of(String string) {
        Atom atom = AtomRegistry.instance().get(string);
        if (atom == null) {
            atom = new Atom(string);
            AtomRegistry.register(atom);
        }
        return atom;
    }

}
