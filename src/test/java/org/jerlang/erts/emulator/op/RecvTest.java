package org.jerlang.erts.emulator.op;

import static org.junit.Assert.assertEquals;

import org.jerlang.erts.Erlang;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.Atom;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.junit.Test;

/**
 * Test for opcodes `recv_mark` and `recv_set`.
 */
public class RecvTest extends AbstractOpTest {

    public RecvTest() {
        super("recv.beam");
    }

    @Test
    public void test_recv() throws ThrowException {
        Term result = Erlang.apply(Atom.of("recv"), Atom.of("test"), List.nil);
        assertEquals(Integer.of(246), result);
    }

}
