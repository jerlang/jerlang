package org.jerlang.stdlib.beam_lib;

import org.jerlang.type.Str;

public enum ChunkId {

    ABST("Abst"),
    ATOM("Atom"),
    ATTR("Attr"),
    CINF("CInf"),
    CODE("Code"),
    EXPT("ExpT"),
    FUNT("FunT"),
    IMPT("ImpT"),
    LINE("Line"),
    LITT("LitT"),
    LOCT("LocT"),
    STRT("StrT");

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
        for (ChunkId chunkIdentifier : values()) {
            if (chunkIdentifier.value == bytes) {
                return chunkIdentifier;
            }
        }
        System.err.println("Invalid chunk ID: " + String.format("%x", bytes));
        return null;
    }

}
