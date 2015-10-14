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

    public String string() {
        return string;
    }

    @Override
    public byte[] toByteArray() {
        return string.getBytes();
    }

    public Str toStr() {
        return this;
    }

    @Override
    public String toString() {
        if (string.matches("\\p{C}")) {
            return super.toString();
        }
        return "\"" + string.replace("\n", "\\n") + "\"";
    }

    public static Str of(String string) {
        return new Str(string);
    }

    public static Str of(byte[] bytes) {
        return new Str(new String(bytes));
    }

    public static Str convert(List list) {
        StringBuilder sb = new StringBuilder(list.length());
        while (list.length() > 0) {
            sb.append((char) list.head().toInteger().toInt());
            list = list.tail();
        }
        return new Str(sb.toString());
    }

}
