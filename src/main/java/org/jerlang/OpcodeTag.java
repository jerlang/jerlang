package org.jerlang;

/**
 * See:
 * lib/compiler/src/beam_opcodes.hrl
 */
public enum OpcodeTag {

    u, // 0
    i, // 1
    a, // 2: atom
    x, // 3
    y, // 4
    f, // 5
    h, // 6
    z; // 7: extended

    private static OpcodeTag[] tags = { u, i, a, x, y, f, h, z };

    public static OpcodeTag decode(int value) {
        return tags[value & 0b111];
    }

}
