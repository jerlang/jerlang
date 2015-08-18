package org.jerlang.type;

import java.util.concurrent.ScheduledFuture;

/**
 * A timer reference.
 */
public class TimerReference extends Reference {

    private final ScheduledFuture<?> future;

    public TimerReference(ScheduledFuture<?> future) {
        super();
        this.future = future;
    }

    public ScheduledFuture<?> future() {
        return future;
    }

    public TimerReference toTimerReference() {
        return this;
    }

}
