package org.jerlang.type;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;

public class BitStringOutputStream extends OutputStream {

    private ByteArrayOutputStream bytes = new ByteArrayOutputStream();
    private int currentByte = 0;
    private int bitOffset = 0;

    public BitString toBitString() {
        if (bitOffset == 0) {
            return new Binary(bytes.toByteArray());
        } else {
            // Flush the last byte
            bytes.write(currentByte);
            return new BitString(bytes.toByteArray(), 8 - bitOffset);
        }
    }

    @Override
    public void write(int b) throws IOException {
        if (bitOffset == 0) {
            bytes.write(b);
        } else {
            // So we want to write a byte at a non-byte boundary
            for (int i = 7; i >= 0; i--) {
                writeBit(((b & 0b1000_0000) >> i) & 0b1);
            }
        }
    }

    public void write(Term term) throws IOException {
        if (term instanceof Integer) {
            write(term.toInteger().toInt());
        } else if (term instanceof BitString) {
            writeBitString(term.toBitString());
        } else {
            throw new IOException("Unsupported term: " + term);
        }
    }

    private void writeBitString(BitString bitString) throws IOException {
        if (bitString.bits() < 8) {
            writeBits(bitString.bytes()[0], bitString.bits());
        } else {
            if (bitString.bits() % 8 == 0) {
                for (int b : bitString.bytes) {
                    write(b);
                }
            } else {
                int lastIndex = bitString.bytes.length - 1;
                for (int index = 0; index < lastIndex; index++) {
                    write(bitString.bytes[index]);
                }
                writeBits(bitString.bytes[lastIndex], bitString.bits() % 8);
            }
        }
    }

    private void writeBit(int bit) throws IOException {
        if (bit != 0) {
            currentByte |= 0b1000_0000 >> bitOffset;
        }
        bitOffset++;
        if (bitOffset == 8) {
            bytes.write(currentByte);
            bitOffset = 0;
            currentByte = 0;
        }
    }

    private void writeBits(int b, int bits) throws IOException {
        // System.out.println("write bits: " + b + ", " + bits);
        int value = b >> (8 - bits);
        while (bits > 0) {
            bits--;
            writeBit((value >> bits) & 0b1);
        }
    }

}
