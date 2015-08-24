package org.jerlang.erts.emulator.op;

import static org.junit.Assert.assertEquals;

import org.jerlang.erts.Erlang;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.Atom;
import org.jerlang.type.Binary;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.junit.Test;

public class Bitsyntax1Test extends AbstractOpTest {

    public Bitsyntax1Test() {
        super("bitsyntax1.beam");
    }

    @Test
    public void test_bitsyntax1() throws ThrowException {
        Binary expected = new Binary(new byte[] { 17, 34, 51 });
        List params = List.nil;
        Term result = Erlang.apply(Atom.of("bitsyntax1"), Atom.of("test"), params);
        assertEquals(expected, result);
    }

}
