package org.jerlang.erts;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.HashSet;

import org.junit.Test;

public class ExternalTermFormatTagTest {

    @Test
    public void testAllTagsAreUnique() {
        HashSet<Integer> tags = new HashSet<>();
        for (ExternalTermFormatTag tag : ExternalTermFormatTag.values()) {
            assertTrue(tags.add(tag.tag()));
        }
    }

    @Test
    public void testAllTagsAreDefined() {
        char[] chars = "abcdefghijklmnopqrstuvwDFIJKLMPR".toCharArray();
        assertEquals(32, chars.length);
        assertEquals(32, ExternalTermFormatTag.values().length);
        for (char c : chars) {
            assertNotNull("Not defined: " + c, ExternalTermFormatTag.of(c));
        }
    }

}
