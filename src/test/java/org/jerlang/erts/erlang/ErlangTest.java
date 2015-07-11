package org.jerlang.erts.erlang;

import static org.junit.Assert.assertEquals;

import org.jerlang.erts.Erlang;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.junit.Test;

public class ErlangTest {

    @Test
    public void testLength1() {
        Integer zero = Integer.of(0);
        assertEquals(zero, Erlang.length(List.nil));
        assertEquals(zero, Erlang.length(List.of()));
        assertEquals(Integer.of(1), Erlang.length(List.of(zero)));
        assertEquals(Integer.of(2), Erlang.length(List.of(zero, zero)));
    }

}
