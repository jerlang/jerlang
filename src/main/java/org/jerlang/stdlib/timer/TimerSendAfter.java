package org.jerlang.stdlib.timer;

import org.jerlang.ProcessRegistry;
import org.jerlang.VirtualMachine;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.PID;
import org.jerlang.type.Term;

public class TimerSendAfter {

    private TimerSendAfter() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 2:
            Integer time2 = params.head().toInteger();
            params = params.tail();
            Term message2 = params.head();
            return send_after_2(time2, message2);
        case 3:
            Integer time3 = params.head().toInteger();
            params = params.tail();
            Term message3 = params.head();
            PID pid3 = ProcessRegistry.self().pid();
            return send_after_3(time3, pid3, message3);
        default:
            throw new Error("badarg");
        }
    }

    /**
     * Same as send_after(Time, self(), Message).
     *
     * http://www.erlang.org/doc/man/timer.html#send_after-2
     */
    public static Term send_after_2(Integer time, Term message) {
        return send_after_3(time, ProcessRegistry.self().pid(), message);
    }

    /**
     * Evaluates Pid ! Message after Time amount of time has elapsed.
     * (Pid can also be an atom of a registered name.)
     * Returns {ok, TRef}, or {error, Reason}.
     *
     * http://www.erlang.org/doc/man/timer.html#send_after-3
     */
    public static Term send_after_3(Integer time, PID pid, Term message) {
        return VirtualMachine.instance().send_after(time.toInt(), message, pid);
    }

}
