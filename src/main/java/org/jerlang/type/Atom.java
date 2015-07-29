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

    private final String name;
    private final String value;

    private Atom(String name) {
        this(name, name);
    }

    private Atom(String name, String value) {
        this.name = name;
        this.value = value;
    }

    @Override
    public boolean equals(Object object) {
        if (object instanceof Atom) {
            Atom other = (Atom) object;
            return name.equals(other.name) && value.equals(other.value);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return name.hashCode();
    }

    public boolean isFalse() {
        if (equals(Boolean.am_false)) {
            return true;
        } else if (equals(Boolean.am_true)) {
            return false;
        } else {
            throw new Error("Cannot convert to boolean: " + this);
        }
    }

    public boolean isTrue() {
        if (equals(Boolean.am_false)) {
            return false;
        } else if (equals(Boolean.am_true)) {
            return true;
        } else {
            throw new Error("Cannot convert to boolean: " + this);
        }
    }

    @Override
    public Atom toAtom() {
        return this;
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

    public static Atom of(String name, String value) {
        Atom atom = AtomRegistry.instance().get(name);
        if (atom == null) {
            atom = new Atom(name, value);
            AtomRegistry.register(atom);
        } else if (atom.value.equals(value)) {
            return atom;
        } else {
            throw new Error("Duplicate value of atom: " + name);
        }
        return atom;
    }

}
