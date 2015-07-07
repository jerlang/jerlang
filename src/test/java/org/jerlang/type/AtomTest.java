package org.jerlang.type;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class AtomTest {

    @Test
    public void testEquals() {
        Atom atom1 = Atom.of("atom");
        Atom atom2 = Atom.of("atom");
        assertEquals(atom1, atom2);
    }

    @Test
    public void testToString() {
        assertEquals("atom", Atom.of("atom").toString());
    }

}
