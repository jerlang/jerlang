package org.jerlang.stdlib.beam_lib;

import org.jerlang.type.Str;

public enum ChunkId implements Comparable<ChunkId> {

    ABST("Abst", 10), // Abstract Code
    ATOM("Atom", 1), // Atoms
    ATTR("Attr", 11), // Attributes
    CINF("CInf", 12), // Compile Information
    CODE("Code", 8), // Code
    EXPT("ExpT", 4), // Export Table
    FUNT("FunT", 5), // Function Table
    IMPT("ImpT", 6), // Import Table
    LINE("Line", 9), // Line Table
    LITT("LitT", 2), // Literal Table
    LOCT("LocT", 7), // Local Table
    STRT("StrT", 3), // String Table
    // See lib/kernel/src/hipe_unified_loader.erl:
    HA64("HA64", -1), // HiPE, x86_64, (implicit: 64-bit, Unix)
    HARM("HARM", -1), // HiPE, arm, v5 (implicit: 32-bit, Linux)
    HPPC("HPPC", -1), // HiPE, PowerPC (implicit: 32-bit, Linux)
    HP64("HP64", -1), // HiPE, ppc64 (implicit: 64-bit, Linux)
    HS8P("HS8P", -1), // HiPE, SPARC, V8+ (implicit: 32-bit)
    HX86("HX86", -1); // HiPE, x86, (implicit: Unix)

    private final int sortOrder; // ascending
    private final String string;
    private final int value;

    private ChunkId(String string, int sortOrder) {
        char a = string.charAt(0);
        char b = string.charAt(1);
        char c = string.charAt(2);
        char d = string.charAt(3);
        this.sortOrder = sortOrder;
        this.string = string;
        this.value = (a << 24) | (b << 16) | (c << 8) | d;
    }

    public boolean skip() {
        return sortOrder == -1;
    }

    public int sortOrder() {
        return sortOrder;
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
