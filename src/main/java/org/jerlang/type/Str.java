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
        String tailString = string.substring(1);
        if (tailString.isEmpty()) {
            return nil;
        } else {
            return new Str(tailString);
        }
    }

}
