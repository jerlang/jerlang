package org.jerlang.erts.emulator.op;

import static org.junit.Assert.assertEquals;

import org.jerlang.erts.Erlang;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.Atom;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.jerlang.vm.VirtualMachine;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class MessageTest {

    @Before
    public void prepare() throws Exception {
        String[] cmd = { "cp", "src/test/resources/msg.beam", "." };
        Runtime.getRuntime().exec(cmd).waitFor();

        VirtualMachine.instance().start();
    }

    @Test
    public void test_msg() throws ThrowException, InterruptedException {
        Term result = Erlang.apply(Atom.of("msg"), Atom.of("test"), List.nil);
        assertEquals(Atom.of("killed"), result);
    }

    @After
    public void cleanup() throws Exception {
        String[] cmd = { "rm", "msg.beam" };
        Runtime.getRuntime().exec(cmd).waitFor();

        VirtualMachine.instance().shutdown();
    }

}
