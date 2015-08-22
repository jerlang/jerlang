package org.jerlang.stdlib.timer;

import org.jerlang.erts.erlang.Error;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.jerlang.type.TimerReference;
import org.jerlang.vm.VirtualMachine;

public class TimerCancel {

    private TimerCancel() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 1:
            return cancel_1(params.head().toTimerReference());
        default:
            throw new Error("badarg");
        }
    }

    /**
     * Cancels a previously requested timeout.
     * TRef is a unique timer reference returned by the timer
     * function in question.
     * Returns {ok, cancel}, or {error, Reason} when TRef is
     * not a timer reference.
     *
     * http://www.erlang.org/doc/man/timer.html#cancel-1
     */
    public static Term cancel_1(TimerReference timerReference) {
        return VirtualMachine.instance().cancel(timerReference);
    }

}
