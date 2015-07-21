package org.jerlang.erts;

import org.junit.Test;

public class EmulatorTest {

    @Test
    public void testParse() {
        Emulator.parse("+W w -sname arnie +R 9 -s my_init -extra +bertie".split(" "));

    }

}
