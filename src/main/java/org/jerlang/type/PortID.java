package org.jerlang.type;

import java.util.Objects;

/**
 * The identifier of a port.
 *
 * See:
 * http://erlang.org/doc/reference_manual/ports.html
 */
public class PortID extends PidOrPortId {

    private final int processId;

    public PortID(int processId) {
        this.processId = processId;
    }

    @Override
    public boolean equals(Object object) {
        if (object instanceof PortID) {
            PortID other = (PortID) object;
            return processId == other.processId;
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Objects.hash(processId);
    }

    @Override
    public PortID toPortID() {
        return this;
    }

    @Override
    public String toString() {
        return "#Port<0." + processId + ">";
    }

}
