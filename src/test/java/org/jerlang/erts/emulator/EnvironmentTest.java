package org.jerlang.erts.emulator;

import static org.assertj.core.api.StrictAssertions.assertThat;

import org.junit.Test;

public class EnvironmentTest {

    @Test
    public void testEnvironment() {
        test(Environment.ERL_AFLAGS, "ERL_AFLAGS");
        test(Environment.ERL_CRASH_DUMP, "ERL_CRASH_DUMP");
        test(Environment.ERL_CRASH_DUMP_NICE, "ERL_CRASH_DUMP_NICE");
        test(Environment.ERL_CRASH_DUMP_SECONDS, "ERL_CRASH_DUMP_SECONDS");
        test(Environment.ERL_EPMD_ADDRESS, "ERL_EPMD_ADDRESS");
        test(Environment.ERL_EPMD_PORT, "ERL_EPMD_PORT");
        test(Environment.ERL_FLAGS, "ERL_FLAGS");
        test(Environment.ERL_LIBS, "ERL_LIBS");
        test(Environment.ERL_ZFLAGS, "ERL_ZFLAGS");
    }

    private void test(Environment environment, String name) {
        assertThat(environment.get()).isNull();
        assertThat(environment.name()).isEqualTo(name);
    }

}
