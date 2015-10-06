package org.jerlang;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;

import org.jerlang.type.Atom;
import org.junit.Test;

public class ProcessPriorityTest {

    @Test
    public void testPriorities() {
        testPriority(ProcessPriority.MAX, 0, "max");
        testPriority(ProcessPriority.HIGH, 1, "high");
        testPriority(ProcessPriority.NORMAL, 2, "normal");
        testPriority(ProcessPriority.LOW, 3, "low");
    }

    private void testPriority(ProcessPriority priority, int ordinal, String atom) {
        assertThat(priority.ordinal()).isEqualTo(ordinal);
        assertThat(priority.toAtom()).isEqualTo(Atom.of(atom));
    }

}
