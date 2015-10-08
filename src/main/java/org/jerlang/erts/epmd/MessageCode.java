package org.jerlang.erts.epmd;

/**
 * See:
 * http://www.erlang.org/doc/apps/erts/erl_dist_protocol.html
 */
public enum MessageCode {

    // Registration and queries -----------------------------------------------

    /**
     * Register a node in the EPMD.
     *
     * When a distributed node is started it registers itself in EPMD.
     * The message ALIVE2_REQ described below is sent from the node towards
     * EPMD. The response from EPMD is ALIVE2_RESP.
     *
     * The connection created to the EPMD must be kept as long as the node is
     * a distributed node. When the connection is closed the node is
     * automatically unregistered from the EPMD.
     */
    ALIVE2_REQ('x'), // 120
    ALIVE2_RESP('y'), // 121

    /**
     * Get the distribution port of another node.
     *
     * When one node wants to connect to another node it starts with a
     * PORT2_REQ request towards EPMD on the host where the node resides in
     * order to get the distribution port that the node listens to.
     */
    PORT2_REQ('z'), // 122, also named as "PORT_PLEASE2_REQ"
    PORT2_RESP('w'), // 119

    /**
     * Get all registered names from EPMD.
     *
     * This request is used via the Erlang function net_adm:names/1,2.
     * A TCP connection is opened towards EPMD and this request is sent.
     */
    NAMES_REQ('n'), // 110

    // Interactive client command codes ---------------------------------------

    /**
     * Dump all data from EPMD.
     *
     * This request is not really used,
     * it should be regarded as a debug feature.
     */
    DUMP_REQ('d'), // 100

    /**
     * Kill the EPMD.
     *
     * This request will kill the running EPMD. It is almost never used.
     */
    KILL_REQ('k'), // 107

    /**
     * Not Used.
     */
    STOP_REQ('s'); // 115

    private final int code;

    private MessageCode(int code) {
        this.code = code;
    }

    public int toCode() {
        return code;
    }

    public static MessageCode byCode(int code) {
        for (MessageCode messageCode : values()) {
            if (messageCode.code == code) {
                return messageCode;
            }
        }
        return null;
    }

}
