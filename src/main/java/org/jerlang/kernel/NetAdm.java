package org.jerlang.kernel;

import org.jerlang.kernel.net_adm.NetAdmPing;
import org.jerlang.type.Atom;
import org.jerlang.type.Term;

/**
 * = net_adm
 *
 * This module contains various network utility functions
 *
 * http://www.erlang.org/doc/man/net_adm.html
 */
public class NetAdm {

    private NetAdm() {
    }

    public static Term dns_hostname(Term host) {
        return null;
    }

    public static Term host_file() {
        return null;
    }

    public static Term localhost() {
        return null;
    }

    public static Term names() {
        return null;
    }

    public static Term names(Term host) {
        return null;
    }

    public static Atom ping(Atom node) {
        return NetAdmPing.ping_1(node);
    }

    public static Term world() {
        return null;
    }

    public static Term world(Term arg) {
        return null;
    }

    public static Term world_list(Term hosts) {
        return null;
    }

    public static Term world_list(Term hosts, Term arg) {
        return null;
    }

}
