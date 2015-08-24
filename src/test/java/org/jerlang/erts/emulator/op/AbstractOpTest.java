package org.jerlang.erts.emulator.op;

import org.jerlang.vm.VirtualMachine;
import org.junit.After;
import org.junit.Before;

public abstract class AbstractOpTest {

    private final String filename;

    public AbstractOpTest(String filename) {
        this.filename = filename;
    }

    @Before
    public void prepare() throws Exception {
        String[] cmd = { "cp", "src/test/resources/" + filename, "." };
        Runtime.getRuntime().exec(cmd).waitFor();

        VirtualMachine.instance().start();
    }

    @After
    public void cleanup() throws Exception {
        String[] cmd = { "rm", filename };
        Runtime.getRuntime().exec(cmd).waitFor();

        VirtualMachine.instance().shutdown();
    }

}
