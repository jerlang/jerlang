package org.jerlang.type;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class AtomTest {

    @Test
    public void testEquals() {
        Atom atom1 = new Atom("atom");
        Atom atom2 = new Atom("atom");
        assertEquals(atom1, atom2);
    }

    @Test
    public void testToString() {
        assertEquals("atom", new Atom("atom").toString());
    }

}
