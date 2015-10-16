package org.jerlang;

import org.jerlang.type.PortID;

public abstract class Port extends ProcessOrPort {

    public Port(PortID portId) {
        super(portId);
    }

}
