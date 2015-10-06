package org.jerlang.kernel;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;

import org.jerlang.erts.Erlang;
import org.jerlang.type.Atom;
import org.jerlang.type.List;
import org.jerlang.type.Str;
import org.jerlang.type.Term;
import org.junit.Test;

public class OSTest {

    @Test
    public void testGetenv0() {
        List env = OS.getenv();
        assertThat(env).isNotNull();
        assertThat(Erlang.length(env).toInt()).isGreaterThan(0);
    }

    @Test
    public void testGetenv1() {
        Term env1 = OS.getenv(Str.of("DOES_NOT_EXIST"));
        assertThat(env1).isInstanceOf(Atom.class);

        Term env2 = OS.getenv(Str.of("PATH"));
        assertThat(env2).isInstanceOf(Str.class);
    }

    @Test
    public void testGetenv2() {
        Atom def = Atom.of("default");
        Term env = OS.getenv(Str.of("DOES_NOT_EXIST"), def);
        assertThat(env).isEqualTo(def);
    }

    @Test
    public void testGetpid0() {
        Str pidStr = OS.getpid();
        assertThat(pidStr).isNotNull();
        assertThat(Long.parseLong(pidStr.string())).isGreaterThan(0);
    }

    @Test
    public void testPutenv2() {
        Str key = Str.of("TEST_KEY_PUTENV_2");
        Str val = Str.of("TEST_VAL_PUTENV_2");
        assertThat(OS.getenv(key)).isNotEqualTo(val);
        OS.putenv(key, val);
        assertThat(OS.getenv(key)).isEqualTo(val);
    }

}
