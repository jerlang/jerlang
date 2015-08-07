package org.jerlang;

import org.jerlang.type.Atom;

/**
 * See:
 * lib/compiler/src/beam_opcodes.hrl
 */
public enum OpcodeTag {

    u, // 0
    i, // 1
    a, // 2: atom
    x, // 3: X register
    y, // 4: Y register (Stack)
    f, // 5: Label
    h, // 6
    z; // 7: extended

    private static OpcodeTag[] tags = { u, i, a, x, y, f, h, z };

    public static OpcodeTag decode(int value) {
        return tags[value & 0b111];
    }

    public Atom toAtom() {
        return Atom.of(name());
    }

}
