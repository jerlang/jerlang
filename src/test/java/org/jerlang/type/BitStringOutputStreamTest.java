package org.jerlang.type;

import static org.junit.Assert.assertEquals;

import java.io.IOException;

import org.junit.Before;
import org.junit.Test;

public class BitStringOutputStreamTest {

    private BitStringOutputStream bsos;

    @Before
    public void setup() {
        bsos = new BitStringOutputStream();
    }

    @Test
    public void testWriteOneByte() throws IOException {
        bsos.write(1);
        BitString expected = new BitString(new byte[] { 1 });
        assertEquals(expected, bsos.toBitString());
    }

    @Test
    public void testWriteTwoBytes() throws IOException {
        bsos.write(1);
        bsos.write(255);
        BitString expected = new BitString(new int[] { 1, 255 });
        assertEquals(expected, bsos.toBitString());
    }

    @Test
    public void testWriteOneBit() throws IOException {
        BitString bitString = new BitString(new int[] { 1 << 7 }, 7);
        bsos.write(bitString);
        assertEquals(bitString, bsos.toBitString());
    }

    @Test
    public void testWrite2ByteBitString() throws IOException {
        BitString bitString = new BitString(new int[] { 1, 255 });
        bsos.write(bitString);
        assertEquals(bitString, bsos.toBitString());
    }

    @Test
    public void testWrite2BytePlus1BitBitString() throws IOException {
        BitString bitString = new BitString(new int[] { 1, 255, 1 << 7 }, 7);
        bsos.write(bitString);
        assertEquals(bitString, bsos.toBitString());
    }

}
