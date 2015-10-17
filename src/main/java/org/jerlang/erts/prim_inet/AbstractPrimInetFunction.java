package org.jerlang.erts.prim_inet;

import org.jerlang.erts.Erlang;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Atom;
import org.jerlang.type.List;
import org.jerlang.type.PortID;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;

/**
 * Base class for all prim_inet functions.
 *
 * Contains non-exported functions that are used by at least two
 * functions of the prim_inet module.
 */
public abstract class AbstractPrimInetFunction {

    private static final Atom error = Atom.of("error");
    private static final Atom einval = Atom.of("einval");
    private static final Atom inet_reply = Atom.of("inet_reply");
    private static final Atom ok = Atom.of("ok");

    protected static Term ctl_cmd(PortID socket, Term cmd, Term args) {
        try {
            Term result = Erlang.port_control(socket, cmd.toInteger(), args);
            List list = result.toList();
            Term reply = list.head();
            if (InetReply.INET_REP_OK.equals(reply)) {
                return Tuple.of(ok, list.tail());
            }
            if (InetReply.INET_REP.equals(reply)) {
                return inet_reply;
            }
            if (InetReply.INET_REP_ERROR.equals(reply)) {
                return Tuple.of(error, Erlang.list_to_atom(list.tail()));
            }
            throw Error.badarg;
        } catch (Exception e) {
            return Tuple.of(error, einval);
        }
    }

}
