package org.jerlang.erts.erlang;

import org.jerlang.type.Atom;
import org.jerlang.type.List;
import org.jerlang.type.PortID;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;
import org.jerlang.vm.VirtualMachine;

public class ErlangOpenPort {

    private static final Atom spawn = Atom.of("spawn");
    private static final Atom spawn_driver = Atom.of("spawn_driver");
    private static final Atom spawn_executable = Atom.of("spawn_executable");

    private ErlangOpenPort() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 3:
            Tuple portName = params.head().toTuple();
            params = params.tail();
            List portSettings = params.head().toList();
            return open_port_2(portName, portSettings);
        default:
            throw Error.badarg;
        }
    }

    /**
     * Returns a port identifier as the result of opening a new Erlang port.
     * A port can be seen as an external Erlang process.
     *
     * http://erlang.org/doc/man/erlang.html#open_port-2
     */
    public static PortID open_port_2(Tuple portName, List portSettings) {
        Atom name = portName.element(1).toAtom();
        if (spawn.equals(name)) {
            Term command = portName.element(2);
            return VirtualMachine.instance().spawn_port(command, portSettings);
        } else if (spawn_driver.equals(name)) {
            Term command = portName.element(2);
            return VirtualMachine.instance().spawn_driver_port(command, portSettings);
        } else if (spawn_executable.equals(name)) {
            Term fileName = portName.element(2);
            return VirtualMachine.instance().spawn_executable_port(fileName, portSettings);
        } else {
            throw Error.badarg;
        }
    }

}
