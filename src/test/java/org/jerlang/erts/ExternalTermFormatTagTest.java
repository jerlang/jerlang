package org.jerlang.erts;

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

}
