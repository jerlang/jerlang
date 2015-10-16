package org.jerlang.erts.prim_inet;

/**
 * See:
 * lib/kernel/src/inet_int.hrl
 */
public enum InetReply {

    INET_REP_ERROR(0),
    INET_REP_OK(1),
    INET_REP(2);

    private final int value;

    private InetReply(int value) {
        this.value = value;
    }

    public int value() {
        return value;
    }

    public static InetReply byValue(int value) {
        for (InetReply inetReply : values()) {
            if (inetReply.value == value) {
                return inetReply;
            }
        }
        return null;
    }

}
