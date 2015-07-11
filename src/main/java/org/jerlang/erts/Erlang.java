package org.jerlang.erts;

import org.jerlang.FunctionSignature;
import org.jerlang.Module;
import org.jerlang.ModuleRegistry;
import org.jerlang.type.Atom;
import org.jerlang.type.Binary;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Str;
import org.jerlang.type.Term;

/**
 * http://www.erlang.org/doc/man/erlang.html
 */
public class Erlang {

    static {
        ModuleRegistry.register("erlang")
            .export("abs", 1)
            .export("apply", 3)
            .export("display", 1)
            .export("function_exported", 3)
            .export("halt", 1)
            .export("integer_to_binary", 1)
            .export("integer_to_binary", 2)
            .export("integer_to_list", 1)
            .export("integer_to_list", 2)
            .export("length", 1);
    }

    /**
     * abs(Float) -> float()
     * abs(Int) -> integer() >= 0
     *
     * Types:
     * Float = float()
     * Int = integer()
     *
     * Returns an integer or float which is the arithmetical absolute
     * value of Float or Int.
     *
     * > abs(-3.33).
     * 3.33
     * > abs(-3).
     * 3
     *
     * Allowed in guard tests.
     */
    public static Integer abs(Integer integer) {
        return new Integer(integer.toBigInteger().abs());
    }

    /**
     * Returns the result of applying Function in Module to Args.
     * The applied function must be exported from Module.
     * The arity of the function is the length of Args.
     *
     * http://www.erlang.org/doc/man/erlang.html#apply-3
     */
    public static Term apply(Term m, Term f, Term a) {
        return null; // FIXME
    }

    /**
     * Prints a text representation of Term on the standard output.
     * On OSE the term is printed to the ramlog.
     *
     * http://www.erlang.org/doc/man/erlang.html#display-1
     */
    public static void display(Term term) {
        System.out.println(term);
    }

    /**
     * Returns true if the module Module is loaded and contains an exported
     * function Function/Arity, or if there is a BIF (a built-in function
     * implemented in C) with the given name; otherwise returns false.
     *
     * http://www.erlang.org/doc/man/erlang.html#function_exported-3
     */
    public static boolean function_exported(Atom module, Atom function, Integer arity) {
        Module m = ModuleRegistry.instance().get(module);
        if (m == null) {
            return false;
        } else {
            return m.hasFunction(new FunctionSignature(module, function, arity));
        }
    }

    /**
     * The same as halt(Status, []).
     *
     * http://www.erlang.org/doc/man/erlang.html#halt-1
     */
    public static void halt(int status) {
        // TODO
    }

    /**
     * Returns a binary which corresponds to the text representation of Integer.
     *
     * http://www.erlang.org/doc/man/erlang.html#integer_to_binary-1
     */
    public static Binary integer_to_binary(Integer integer) {
        return new Binary(integer.toString().getBytes());
    }

    /**
     * http://www.erlang.org/doc/man/erlang.html#integer_to_binary-2
     */
    public static Binary integer_to_binary(Integer integer, Integer base) {
        return new Binary(integer.toBigInteger().toString(base.toInt()).getBytes());
    }

    /**
     * Returns a string which corresponds to the text representation of Integer.
     *
     * http://www.erlang.org/doc/man/erlang.html#integer_to_list-1
     */
    public static List integer_to_list(Integer integer) {
        return new Str(integer.toString());
    }

    /**
     * Returns a string which corresponds to the text representation of Integer
     * in base Base.
     *
     * http://www.erlang.org/doc/man/erlang.html#integer_to_list-2
     */
    public static List integer_to_list(Integer integer, Integer base) {
        return new Str(integer.toBigInteger().toString(base.toInt()));
    }

    /**
     * Returns the length of List.
     *
     * http://www.erlang.org/doc/man/erlang.html#length-1
     */
    public static Integer length(List list) {
        if (list == List.nil) {
            return new Integer(0);
        }
        int length = 0;
        while (list.head() != null) {
            list = list.tail();
            length++;
        }
        return new Integer(length);
    }

}
