package org.jerlang.erts;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;

import org.jerlang.FunctionSignature;
import org.junit.Test;

public class EmulatorTest {

    @Test
    public void testParse() {
        Emulator.parse("+W w -sname arnie +R 9 -s my_init -extra +bertie".split(" "));
        assertThat(Runtime.getRunFlag())
            .isEqualTo(new FunctionSignature("my_init", "start", 0));
    }

}
