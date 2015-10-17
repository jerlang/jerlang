package org.jerlang;

import java.util.Objects;

import org.jerlang.erts.erlang.Error;
import org.jerlang.type.PID;
import org.jerlang.type.PidOrPortId;
import org.jerlang.type.PortID;
import org.jerlang.vm.Scheduler;

/**
 * Base class for processes and ports.
 */
public abstract class ProcessOrPort {

    private PidOrPortId id;
    private ProcessPriority priority = ProcessPriority.NORMAL;
    private ProcessState state = ProcessState.RUNNABLE;
    private Scheduler scheduler = null;

    public ProcessOrPort(PidOrPortId id) {
        this.id = id;
    }

    public abstract void execute();

    public PidOrPortId id() {
        return id;
    }

    public PID pid() {
        return id.toPID();
    }

    public PortID portId() {
        return id.toPortID();
    }

    public ProcessPriority priority() {
        return priority;
    }

    public void setPriority(ProcessPriority priority) {
        this.priority = Objects.requireNonNull(priority);
    }

    public void setScheduler(Scheduler scheduler) {
        this.scheduler = scheduler;
    }

    public void setState(ProcessState state) {
        this.state = state;
    }

    protected Scheduler scheduler() {
        return scheduler;
    }

    public ProcessState state() {
        return state;
    }

    public Port toPort() {
        throw new Error("can not convert to port: " + this);
    }

    public Process toProcess() {
        throw new Error("can not convert to process: " + this);
    }

}
