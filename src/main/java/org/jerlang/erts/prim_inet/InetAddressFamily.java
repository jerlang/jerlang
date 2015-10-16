package org.jerlang.erts.prim_inet;

import org.jerlang.type.Atom;

public enum InetAddressFamily {

    INET_AF_INET(1, "inet"),
    INET_AF_INET6(2, "inet6"),
    INET_AF_ANY(3, "any"), // Fake for ANY in any address family
    INET_AF_LOOPBACK(4, "loopback"); //Fake for LOOPBACK in any address family

    private final Atom atom;
    private final int value;

    private InetAddressFamily(int value, String atom) {
        this.atom = Atom.of(atom);
        this.value = value;
    }

    public int value() {
        return value;
    }

    public static InetAddressFamily byValue(int value) {
        for (InetAddressFamily inetAddressFamily : values()) {
            if (inetAddressFamily.value == value) {
                return inetAddressFamily;
            }
        }
        return null;
    }

    public static InetAddressFamily byAtom(Atom atom) {
        for (InetAddressFamily inetAddressFamily : values()) {
            if (inetAddressFamily.atom.equals(atom)) {
                return inetAddressFamily;
            }
        }
        return null;
    }
}
