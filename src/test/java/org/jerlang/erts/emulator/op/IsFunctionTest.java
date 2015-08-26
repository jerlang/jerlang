package org.jerlang.erts.emulator.op;

import static org.junit.Assert.assertEquals;

import org.jerlang.FunctionSignature;
import org.jerlang.erts.Erlang;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.Atom;
import org.jerlang.type.Boolean;
import org.jerlang.type.Fun;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Map;
import org.jerlang.type.Term;
import org.junit.Test;

public class IsFunctionTest extends AbstractOpTest {

    private FunctionSignature fs1 = new FunctionSignature("m", "f", 1);

    private List params1 = List.of(new Fun(null, null));
    private List params2 = List.of(new Map());

    private List params3 = List.of(new Fun(fs1, null), Integer.of(1));
    private List params4 = List.of(new Fun(fs1, null), Integer.of(2));
    private List params5 = List.of(new Map(), Integer.of(1));

    public IsFunctionTest() {
        super("is_fun.beam");
    }

    @Test
    public void test1() throws ThrowException {
        Term result1 = Erlang.apply(Atom.of("is_fun"), Atom.of("is_fun1"), params1);
        assertEquals(Boolean.am_true, result1);

        Term result2 = Erlang.apply(Atom.of("is_fun"), Atom.of("is_fun1"), params2);
        assertEquals(Boolean.am_false, result2);
    }

    @Test
    public void test2() throws ThrowException {
        Term result3 = Erlang.apply(Atom.of("is_fun"), Atom.of("is_fun2"), params3);
        assertEquals(Boolean.am_true, result3);

        Term result4 = Erlang.apply(Atom.of("is_fun"), Atom.of("is_fun2"), params4);
        assertEquals(Boolean.am_false, result4);

        Term result5 = Erlang.apply(Atom.of("is_fun"), Atom.of("is_fun2"), params5);
        assertEquals(Boolean.am_false, result5);
    }

}
