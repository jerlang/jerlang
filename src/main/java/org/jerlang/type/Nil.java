package org.jerlang.type;

public class Nil extends List {

    public Nil() {
        super(null);
    }

    @Override
    public int length() {
        return 0;
    }

}
