package org.jerlang.erts.prim_inet;

import static org.jerlang.erts.PrimInet.close;
import static org.jerlang.erts.PrimInet.setopts;
import static org.jerlang.type.List.nil;

import org.jerlang.erts.Erlang;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Atom;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.PortID;
import org.jerlang.type.Str;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;

public class PrimInetOpen extends AbstractPrimInetFunction {

    private static final Atom ok = Atom.of("ok");

    private static final Atom tcp = Atom.of("tcp");
    private static final Atom udp = Atom.of("udp");
    private static final Atom sctp = Atom.of("sctp");

    private PrimInetOpen() {
    }

    public static Term dispatch(List params) {
        Atom protocol = params.head().toAtom();
        params = params.tail();
        Atom family = params.head().toAtom();
        params = params.tail();
        Atom type = params.head().toAtom();
        switch (params.length()) {
        case 3:
            return open_3(protocol, family, type);
        case 4:
            params = params.tail();
            List opts = params.head().toList();
            return open_4(protocol, family, type, opts);
        default:
            throw new Error("badarg");
        }
    }

    public static Tuple open_3(Atom protocol, Atom family, Atom type) {
        return open_4(protocol, family, type, nil);
    }

    public static Tuple open_4(Atom protocol, Atom family, Atom type, List opts) {
        Str driver = protocol2drv(protocol);
        Integer af = enc_family(family);
        Integer t = enc_type(type);
        Tuple portName = Tuple.of(Atom.of("spawn_driver"), driver);
        List portSettings = List.of(Atom.of("binary"));
        PortID s = Erlang.open_port(portName, portSettings);
        Term r1 = setopts(s, opts);
        if (ok.equals(r1)) {
            Tuple r2 = ctl_cmd(s, InetRequest.INET_REQ_OPEN.value(), List.of(af, t, opts));
            if (ok.equals(r2.element(1))) {
                return Tuple.of(ok, s);
            } else {
                close(s);
                return r2;
            }
        } else {
            close(s);
            return r1.toTuple();
        }
    }

    private static Integer enc_family(Atom family) {
        InetAddressFamily inetAddressFamily = InetAddressFamily.byAtom(family);
        if (inetAddressFamily == null) {
            throw new Error("badmatch");
        }
        return Integer.of(inetAddressFamily.value());
    }

    private static Integer enc_type(Atom type) {
        InetType inetType = InetType.byAtom(type);
        if (inetType == null) {
            throw new Error("badmatch");
        }
        return Integer.of(inetType.value());
    }

    private static Str protocol2drv(Atom protocol) {
        if (tcp.equals(protocol)) {
            return Str.of("tcp_inet");
        }
        if (udp.equals(protocol)) {
            return Str.of("udp_inet");
        }
        if (sctp.equals(protocol)) {
            return Str.of("sctp_inet");
        }
        throw new Error("badmatch");
    }

    private static Tuple ctl_cmd(PortID socket, int req, List data) {
        return null;
    }

}
