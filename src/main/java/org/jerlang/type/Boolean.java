package org.jerlang.type;

/**
 * There is no real boolean type in Erlang, only the atoms `true` and `false`.
 * This class defines both atoms and a boolean-to-Atom conversion function.
 *
 * NOTE: This class is in the `org.jerlang.type` package,
 * but it does not define a type and
 * therefore does not extend from `org.jerlang.type.Term`.
 */
public class Boolean {

    public static final Atom am_true = Atom.of("true");
    public static final Atom am_false = Atom.of("false");

    private Boolean() {
    }

    public static Atom of(boolean value) {
        return value ? am_true : am_false;
    }

}
