package org.jerlang.erts;

import org.jerlang.erts.erlang.Error;
import org.jerlang.erts.prim_inet.PrimInetClose;
import org.jerlang.erts.prim_inet.PrimInetGetopt;
import org.jerlang.erts.prim_inet.PrimInetGetopts;
import org.jerlang.erts.prim_inet.PrimInetOpen;
import org.jerlang.erts.prim_inet.PrimInetSetopt;
import org.jerlang.erts.prim_inet.PrimInetSetopts;
import org.jerlang.type.Atom;
import org.jerlang.type.List;
import org.jerlang.type.PortID;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;

/**
 * = prim_inet
 *
 * == Description
 *
 * Primitive inet_drv interface
 */
public class PrimInet {

    public static Tuple open(Atom protocol, Atom family, Atom type) {
        return PrimInetOpen.open_3(protocol, family, type);
    }

    public static Tuple open(Atom protocol, Atom family, Atom type, List opts) {
        return PrimInetOpen.open_4(protocol, family, type, opts);
    }

    public static Term fdopen(Term protocol, Term family, Term type, Term fd) {
        throw new Error("not implemented");
    }

    public static Term fdopen(Term protocol, Term family, Term type, Term fd, Term bound) {
        throw new Error("not implemented");
    }

    public static Term close(PortID socket) {
        return PrimInetClose.close_1(socket);
    }

    public static Term bind(Term socket, Term ip, Term port) {
        throw new Error("not implemented");
    }

    public static Term listen(Term a) {
        throw new Error("not implemented");
    }

    public static Term listen(Term a, Term b) {
        throw new Error("not implemented");
    }

    public static Term peeloff(Term a, Term b) {
        throw new Error("not implemented");
    }

    public static Term connect(Term socket, Term ip, Term port) {
        throw new Error("not implemented");
    }

    public static Term connect(Term socket, Term ip, Term port, Term time) {
        throw new Error("not implemented");
    }

    public static Term async_connect(Term socket, Term ip, Term port, Term time) {
        throw new Error("not implemented");
    }

    public static Term accept(Term socket) {
        throw new Error("not implemented");
    }

    public static Term accept(Term socket, Term time) {
        throw new Error("not implemented");
    }

    public static Term async_accept(Term socket, Term time) {
        throw new Error("not implemented");
    }

    public static Term shutdown(Term socket, Term atom) {
        throw new Error("not implemented");
    }

    public static Term send(Term socket, Term data) {
        throw new Error("not implemented");
    }

    public static Term send(Term socket, Term data, Term optList) {
        throw new Error("not implemented");
    }

    public static Term sendto(Term socket, Term ip, Term port, Term data) {
        throw new Error("not implemented");
    }

    public static Term sendmsg(Term socket, Term sndRcvInfo, Term data) {
        throw new Error("not implemented");
    }

    public static Term recv(Term socket, Term length) {
        throw new Error("not implemented");
    }

    public static Term recv(Term socket, Term length, Term time) {
        throw new Error("not implemented");
    }

    public static Term async_recv(Term socket, Term length, Term time) {
        throw new Error("not implemented");
    }

    public static Term unrecv(Term socket, Term data) {
        throw new Error("not implemented");
    }

    public static Term recvfrom(Term socket, Term length) {
        throw new Error("not implemented");
    }

    public static Term recvfrom(Term socket, Term length, Term time) {
        throw new Error("not implemented");
    }

    public static Term setopt(PortID socket, Term opt, Term value) {
        return PrimInetSetopt.setopt_3(socket, opt, value);
    }

    public static Term setopts(PortID socket, List opts) {
        return PrimInetSetopts.setopts_2(socket, opts);
    }

    public static Term getopt(PortID socket, Atom opt) {
        return PrimInetGetopt.getopt_2(socket, opt);
    }

    public static Term getopts(PortID socket, List opts) {
        return PrimInetGetopts.getopts_2(socket, opts);
    }

    public static Term is_sockopt_val(Term opt, Term val) {
        throw new Error("not implemented");
    }

    public static Term chgopt(Term socket, Term opt, Term value) {
        throw new Error("not implemented");
    }

    public static Term chgopts(Term socket, List opts) {
        throw new Error("not implemented");
    }

    public static Term getstat(Term socket, List stats) {
        throw new Error("not implemented");
    }

    public static Term getfd(Term socket) {
        throw new Error("not implemented");
    }

    public static Term ignorefd(Term socket, Term bool) {
        throw new Error("not implemented");
    }

    public static Term getindex(Term socket) {
        throw new Error("not implemented");
    }

    public static Term getstatus(Term socket) {
        throw new Error("not implemented");
    }

    public static Term gettype(Term socket) {
        throw new Error("not implemented");
    }

    public static Term getifaddrs(Term socket) {
        throw new Error("not implemented");
    }

    public static Term getiflist(Term socket) {
        throw new Error("not implemented");
    }

    public static Term ifget(Term socket, Term name, Term opts) {
        throw new Error("not implemented");
    }

    public static Term ifset(Term socket, Term name, Term opts) {
        throw new Error("not implemented");
    }

    public static Term gethostname(Term socket) {
        throw new Error("not implemented");
    }

    public static Term getservbyname(Term socket, Term name, Term proto) {
        throw new Error("not implemented");
    }

    public static Term getservbyport(Term socket, Term port, Term proto) {
        throw new Error("not implemented");
    }

    public static Term peername(Term socket) {
        throw new Error("not implemented");
    }

    public static Term setpeername(Term socket, Term ipPort) {
        throw new Error("not implemented");
    }

    public static Term peernames(Term socket) {
        throw new Error("not implemented");
    }

    public static Term peernames(Term socket, Term assocId) {
        throw new Error("not implemented");
    }

    public static Term sockname(Term socket) {
        throw new Error("not implemented");
    }

    public static Term setsockname(Term socket, Term ipPort) {
        throw new Error("not implemented");
    }

    public static Term socknames(Term socket) {
        throw new Error("not implemented");
    }

    public static Term socknames(Term socket, Term assocId) {
        throw new Error("not implemented");
    }

    public static Term attach(Term socket) {
        throw new Error("not implemented");
    }

    public static Term detach(Term socket) {
        throw new Error("not implemented");
    }

}
