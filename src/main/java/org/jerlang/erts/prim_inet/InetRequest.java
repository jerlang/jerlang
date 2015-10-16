package org.jerlang.erts.prim_inet;

public enum InetRequest {

    INET_REQ_OPEN(1),
    INET_REQ_CLOSE(2),
    INET_REQ_CONNECT(3),
    INET_REQ_PEER(4),
    INET_REQ_NAME(5),
    INET_REQ_BIND(6),
    INET_REQ_SETOPTS(7),
    INET_REQ_GETOPTS(8),
    INET_REQ_GETIX(9),
    // INET_REQ_GETIF(10) OBSOLETE
    INET_REQ_GETSTAT(11),
    INET_REQ_GETHOSTNAME(12),
    INET_REQ_FDOPEN(13),
    INET_REQ_GETFD(14),
    INET_REQ_GETTYPE(15),
    INET_REQ_GETSTATUS(16),
    INET_REQ_GETSERVBYNAME(17),
    INET_REQ_GETSERVBYPORT(18),
    INET_REQ_SETNAME(19),
    INET_REQ_SETPEER(20),
    INET_REQ_GETIFLIST(21),
    INET_REQ_IFGET(22),
    INET_REQ_IFSET(23),
    INET_REQ_SUBSCRIBE(24),
    INET_REQ_GETIFADDRS(25),
    INET_REQ_ACCEPT(26),
    INET_REQ_LISTEN(27),
    INET_REQ_IGNOREFD(28),
    INET_REQ_GETLADDRS(29),
    INET_REQ_GETPADDRS(30),
    // TCP requests
    // TCP_REQ_ACCEPT(40) MOVED
    // TCP_REQ_LISTEN(41) MERGED
    TCP_REQ_RECV(42),
    TCP_REQ_UNRECV(43),
    TCP_REQ_SHUTDOWN(44),
    // UDP and SCTP requests
    PACKET_REQ_RECV(60),
    // SCTP_REQ_LISTEN(61) MERGED
    SCTP_REQ_BINDX(62), // Multi-home SCTP bind
    SCTP_REQ_PEELOFF(63);

    private final int value;

    private InetRequest(int value) {
        this.value = value;
    }

    public int value() {
        return value;
    }

    public static InetRequest byValue(int value) {
        for (InetRequest inetRequest : values()) {
            if (inetRequest.value == value) {
                return inetRequest;
            }
        }
        return null;
    }

}
