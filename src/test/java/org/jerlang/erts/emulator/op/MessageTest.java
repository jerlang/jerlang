package org.jerlang.erts.emulator.op;

import static org.junit.Assert.assertEquals;

import org.jerlang.erts.Erlang;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.Atom;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.junit.Test;

public class MessageTest extends AbstractOpTest {

    public MessageTest() {
        super("msg.beam");
    }

    @Test
    public void test_msg() throws ThrowException, InterruptedException {
        Term result = Erlang.apply(Atom.of("msg"), Atom.of("test"), List.nil);
        assertEquals(Atom.of("killed"), result);
    }

}
