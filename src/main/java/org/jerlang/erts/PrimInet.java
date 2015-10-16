package org.jerlang.erts;

import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Atom;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * = prim_inet
 *
 * == Description
 *
 * Primitive inet_drv interface
 */
public class PrimInet {

    public static Term open(Term protocol, Term family, Term type) {
        throw new Error("not implemented");
    }

    public static Term open(Term protocol, Term family, Term type, Term opts) {
        throw new Error("not implemented");
    }

    public static Term fdopen(Term protocol, Term family, Term type, Term fd) {
        throw new Error("not implemented");
    }

    public static Term fdopen(Term protocol, Term family, Term type, Term fd, Term bound) {
        throw new Error("not implemented");
    }

    public static Term close(Term socket) {
        throw new Error("not implemented");
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

    public static Term setopt(Term socket, Term opt, Term value) {
        throw new Error("not implemented");
    }

    public static Term setopts(Term socket, Term opts) {
        throw new Error("not implemented");
    }

    public static Term getopt(Term socket, Atom opt) {
        throw new Error("not implemented");
    }

    public static Term getopts(Term socket, List opts) {
        throw new Error("not implemented");
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
