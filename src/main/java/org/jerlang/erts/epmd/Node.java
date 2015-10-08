package org.jerlang.erts.epmd;

/**
 * Information about an Erlang node.
 * Used by EPMD.
 *
 * See:
 * http://www.erlang.org/doc/apps/erts/erl_dist_protocol.html
 */
public class Node {

    /**
     * The port number on which the node accept connection requests.
     */
    private int portNo;

    /**
     * 77 = normal Erlang node, 72 = hidden node (C-node), ...
     */
    private int nodeType;

    /**
     * 0 = TCP/IPv4, ...
     */
    private int protocol;

    /**
     * The highest distribution version that this node can handle.
     * The value in R6B and later is 5.
     */
    private int highestVersion;

    /**
     * The lowest distribution version that this node can handle.
     * The value in R6B and later is 5.
     */
    private int lowestVersion;

    /**
     * The NodeName as an UTF-8 encoded string.
     */
    private String nodeName;

    /**
     * Extra field.
     */
    private byte[] extra;

    // PORT NUMBER ------------------------------------------------------------

    public int portNo() {
        return portNo;
    }

    public Node portNo(int portNo) {
        this.portNo = portNo;
        return this;
    }

    // NODE TYPE --------------------------------------------------------------

    public int nodeType() {
        return nodeType;
    }

    public Node nodeType(int nodeType) {
        this.nodeType = nodeType;
        return this;
    }

    // PROTOCOL ---------------------------------------------------------------

    public int protocol() {
        return protocol;
    }

    public Node protocol(int protocol) {
        this.protocol = protocol;
        return this;
    }

    // HIGHEST VERSION --------------------------------------------------------

    public int highestVersion() {
        return highestVersion;
    }

    public Node highestVersion(int highestVersion) {
        this.highestVersion = highestVersion;
        return this;
    }

    // LOWEST VERSION ---------------------------------------------------------

    public int lowestVersion() {
        return lowestVersion;
    }

    public Node lowestVersion(int lowestVersion) {
        this.lowestVersion = lowestVersion;
        return this;
    }

    // NODE NAME --------------------------------------------------------------

    public String nodeName() {
        return nodeName;
    }

    public Node nodeName(String nodeName) {
        this.nodeName = nodeName;
        return this;
    }

    // EXTRA ------------------------------------------------------------------

    public byte[] extra() {
        return extra;
    }

    public Node extra(byte[] extra) {
        this.extra = extra;
        return this;
    }

}
