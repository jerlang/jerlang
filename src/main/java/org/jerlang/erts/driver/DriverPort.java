package org.jerlang.erts.driver;

import org.jerlang.Port;
import org.jerlang.ProcessState;
import org.jerlang.type.PortID;

/**
 * A port communicating with a driver.
 */
public class DriverPort extends Port {

    private final Driver driver;

    public DriverPort(PortID portId, Driver driver) {
        super(portId, driver);
        this.driver = driver;
    }

    @Override
    public void execute() {
        // For now, go directly in waiting state, i.e. don't do anything
        // TODO: This must be implemented later
        setState(ProcessState.WAITING);
    }

}
