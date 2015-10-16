package org.jerlang.erts.prim_inet;

import org.jerlang.type.Atom;

public enum InetType {

    INET_TYPE_STREAM(1, "stream"),
    INET_TYPE_DGRAM(2, "dgram"),
    INET_TYPE_SEQPACKET(3, "seqpacket");

    private final Atom atom;
    private final int value;

    private InetType(int value, String atom) {
        this.atom = Atom.of(atom);
        this.value = value;
    }

    public int value() {
        return value;
    }

    public static InetType byValue(int value) {
        for (InetType inetType : values()) {
            if (inetType.value == value) {
                return inetType;
            }
        }
        return null;
    }

    public static InetType byAtom(Atom atom) {
        for (InetType inetType : values()) {
            if (inetType.atom.equals(atom)) {
                return inetType;
            }
        }
        return null;
    }

}
