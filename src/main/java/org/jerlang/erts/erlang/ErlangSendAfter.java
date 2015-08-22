package org.jerlang.erts.erlang;

import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.PID;
import org.jerlang.type.Term;
import org.jerlang.type.TimerReference;
import org.jerlang.vm.VirtualMachine;

public class ErlangSendAfter {

    private ErlangSendAfter() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 3:
            Integer time = params.head().toInteger();
            params = params.tail();
            PID pid = params.head().toPID();
            params = params.tail();
            Term msg = params.head();
            return send_after_3(time, pid, msg);
        default:
            throw new Error("badarg");
        }
    }

    /**
     * http://www.erlang.org/doc/man/erlang.html#self-0
     */
    public static TimerReference send_after_3(Integer time, PID pid, Term message) {
        return VirtualMachine.instance().send_after(time.toInt(), message, pid);
    }

}
