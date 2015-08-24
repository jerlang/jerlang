package org.jerlang.erts.emulator.op;

import static org.junit.Assert.assertEquals;

import org.jerlang.erts.Erlang;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.Atom;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.junit.Test;

public class Bitsyntax2Test extends AbstractOpTest {

    public Bitsyntax2Test() {
        super("bitsyntax2.beam");
    }

    @Test
    public void test_bitsyntax1() throws ThrowException {
        Integer one = Integer.of(1);
        Integer two = Integer.of(2);
        Integer three = Integer.of(3);
        List expected = List.of(one, one, two, two, three, three);
        List params = List.nil;
        Term result = Erlang.apply(Atom.of("bitsyntax2"), Atom.of("test"), params);
        assertEquals(expected, result);
    }

}
