package org.jerlang.erts.emulator.op;

import static org.junit.Assert.assertEquals;

import org.jerlang.erts.Erlang;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.Atom;
import org.jerlang.type.Binary;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.junit.Test;

public class BsInitWritableTest extends AbstractOpTest {

    public BsInitWritableTest() {
        super("bs_init_writable.beam");
    }

    @Test
    public void test_bs_init_writable() throws ThrowException {
        Binary expected = new Binary(new int[] { 97, 98, 99 });
        List params = List.nil;
        Term result = Erlang.apply(Atom.of("bs_init_writable"), Atom.of("test"), params);
        assertEquals(expected, result);
    }

}
