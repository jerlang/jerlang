package org.jerlang.erts;

import static org.assertj.core.api.StrictAssertions.assertThat;

import org.jerlang.type.Atom;
import org.jerlang.type.List;
import org.jerlang.type.Str;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;
import org.junit.Before;
import org.junit.Test;

public class InitTest {

    @Before
    public void setup() {
        Runtime.reset();
    }

    @Test
    public void testGetArgument() {
        Emulator.parse("-a b c -a d".split(" "));
        List expected = List.of(
            List.of(Str.of("b"), Str.of("c")),
            List.of(Str.of("d"))
            );
        Term result = Init.get_argument(Atom.of("a"));
        assertThat(result).isEqualTo(Tuple.of(Atom.of("ok"), expected));
    }

}
