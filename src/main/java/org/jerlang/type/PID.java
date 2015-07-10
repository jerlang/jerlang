package org.jerlang.type;

/**
 * TODO: Work in progress.
 * 
 * See also:
 * https://stackoverflow.com/a/262179
 */
public class PID extends Term {

    private final int processId;

    public PID(int processId) {
        this.processId = processId;
    }

    @Override
    public boolean equals(Object object) {
        if (object instanceof PID) {
            PID other = (PID) object;
            return processId == other.processId;
        }
        return false;
    }

    @Override
    public String toString() {
        return "<0." + processId + ".0>";
    }

}
