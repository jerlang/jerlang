package org.jerlang.stdlib.beam_lib;

import org.jerlang.type.Str;

public enum ChunkId {

    ABST("Abst"), // Abstract Code
    ATOM("Atom"), // Atoms
    ATTR("Attr"), // Attributes
    CINF("CInf"), // Compile Information
    CODE("Code"), // Code
    EXPT("ExpT"), // Export Table
    FUNT("FunT"), // Function Table
    IMPT("ImpT"), // Import Table
    LINE("Line"), // Line Table
    LITT("LitT"), // Literal Table
    LOCT("LocT"), // Local Table
    STRT("StrT"); // String Table

    private final String string;
    private final int value;

    private ChunkId(String string) {
        char a = string.charAt(0);
        char b = string.charAt(1);
        char c = string.charAt(2);
        char d = string.charAt(3);
        this.string = string;
        this.value = (a << 24) | (b << 16) | (c << 8) | d;
    }

    public int toInt() {
        return value;
    }

    public Str toStr() {
        return new Str(string);
    }

    @Override
    public String toString() {
        return string;
    }

    public static ChunkId of(int bytes) {
        for (ChunkId chunkId : values()) {
            if (chunkId.value == bytes) {
                return chunkId;
            }
        }
        System.err.println("Invalid chunk ID: " + String.format("%x", bytes));
        return null;
    }

    public static ChunkId of(String string) {
        for (ChunkId chunkId : values()) {
            if (chunkId.string.equals(string)) {
                return chunkId;
            }
        }
        System.err.println("Invalid chunk ID: '" + string + "'");
        return null;
    }
}
