package org.jerlang.type;

import static org.assertj.core.api.StrictAssertions.assertThat;
import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class ListTest {

    @Test
    public void testListOf() {
        List list = List.of(Str.of("a"), Str.of("b"), Str.of("c"));
        assertEquals("[\"a\",\"b\",\"c\"]", list.toString());
    }

    @Test
    public void testLength() {
        assertThat(List.nil.length()).isEqualTo(0);
        List list = List.of(Str.of("a"), Str.of("b"), Str.of("c"));
        assertThat(list.length()).isEqualTo(3);
    }

}
