package org.jerlang.stdlib;

import org.jerlang.ProcessRegistry;
import org.jerlang.stdlib.timer.TimerCancel;
import org.jerlang.type.Integer;
import org.jerlang.type.PID;
import org.jerlang.type.Term;
import org.jerlang.type.TimerReference;
import org.jerlang.vm.VirtualMachine;

/**
 * = timer
 *
 * Timer Functions
 *
 * http://www.erlang.org/doc/man/timer.html
 */
public class Timer {

    public static Term cancel(TimerReference timerReference) {
        return TimerCancel.cancel_1(timerReference);
    }

    public static Term send_after(Integer time, Term message) {
        return VirtualMachine.instance().send_after(time.toInt(), message, ProcessRegistry.self().pid());
    }

    public static Term send_after(Integer time, PID pid, Term message) {
        return VirtualMachine.instance().send_after(time.toInt(), message, pid);
    }

}
