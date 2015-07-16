package org.jerlang.type;

import java.util.Objects;

public class Str extends List {

    private String string;

    public Str(String string) {
        super(null);
        this.string = string;
    }

    @Override
    public Term head() {
        return new Integer(string.charAt(0));
    }

    @Override
    public List tail() {
        String tailString = string.substring(1);
        if (tailString.isEmpty()) {
            return nil;
        } else {
            return new Str(tailString);
        }
    }

    @Override
    public boolean equals(Object object) {
        if (object instanceof Str) {
            Str other = (Str) object;
            return string.equals(other.string);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Objects.hash(string);
    }

    @Override
    public String toString() {
        return "\"" + string + "\"";
    }

    public static Str of(String string) {
        return new Str(string);
    }

}
