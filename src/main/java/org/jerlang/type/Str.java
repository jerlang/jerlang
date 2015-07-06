package org.jerlang.type;

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
        if (string.isEmpty()) {
            return nil;
        } else {
            return new Str(string.substring(1));
        }
    }

}
