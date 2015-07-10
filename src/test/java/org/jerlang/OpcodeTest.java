package org.jerlang;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class OpcodeTest {

    @Test
    public void testEncode() {
        assertEquals(1, Opcode.label.encode());
    }

    @Test
    public void testDecode() {
        assertEquals(Opcode.label, Opcode.decode(1));
    }

    @Test
    public void testToString() {
        assertEquals("label", Opcode.label.toString());
        assertEquals("try", Opcode._try.toString());
    }

}
