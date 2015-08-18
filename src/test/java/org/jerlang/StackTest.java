package org.jerlang;

import static org.junit.Assert.assertEquals;

import org.jerlang.type.Atom;
import org.jerlang.type.PID;
import org.jerlang.type.Term;
import org.junit.Test;

public class StackTest {

    @Test
    public void testStack() {
        Process p = new Process(new PID(0));
        Term a = Atom.of("a");
        Term b = Atom.of("b");
        Term c = Atom.of("c");
        p.pushStack(a);
        p.pushStack(b);
        p.pushStack(c);
        assertEquals(c, p.popStack());
        assertEquals(b, p.popStack());
        assertEquals(a, p.popStack());
    }

}
