package org.jerlang.erts.erlang;

import org.jerlang.Port;
import org.jerlang.ProcessRegistry;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.PortID;
import org.jerlang.type.Term;

public class ErlangPortControl {

    private ErlangPortControl() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 3:
            Term port = params.head();
            params = params.tail();
            Integer operation = params.head().toInteger();
            params = params.tail();
            Term data = params.head();
            return port_control_3(port, operation, data);
        default:
            throw Error.badarg;
        }
    }

    /**
     * Performs a synchronous control operation on a port.
     * The meaning of Operation and Data depends on the port, i.e.,
     * on the port driver. Not all port drivers support this control feature.
     *
     * Returns: a list of integers in the range 0 through 255, or a binary,
     * depending on the port driver. The meaning of the returned data also
     * depends on the port driver.
     *
     * Failure: badarg if Port is not an open port or the registered name of
     * an open port, if Operation cannot fit in a 32-bit integer, if the port
     * driver does not support synchronous control operations, or if the port
     * driver so decides for any reason (probably something wrong with
     * Operation or Data).
     *
     * http://www.erlang.org/doc/man/erlang.html#port_control-3
     */
    public static Term port_control_3(Term portId, Integer operation, Term data) {
        if (portId instanceof PortID) {
            Port port = ProcessRegistry.resolve(portId.toPortID()).toPort();
            return port.control(operation, data);
        } else {
            throw Error.badarg;
        }
    }

}
