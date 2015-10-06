package org.jerlang.stdlib.beam_lib;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;

import org.junit.Test;

public class ExternalTermReaderTest {

    @Test
    public void testReverseArray() {
        byte[] a = { 1, 2, 3 };
        assertThat(a).isEqualTo(new byte[] { 1, 2, 3 });
        ExternalTermReader.reverse(a);
        assertThat(a).isEqualTo(new byte[] { 3, 2, 1 });
    }

}
