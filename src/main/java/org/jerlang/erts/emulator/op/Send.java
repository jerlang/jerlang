package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.ProcessRegistry;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.List;
import org.jerlang.type.PID;
import org.jerlang.type.Term;

/**
 * Send argument in x(1) as a message to the destination process in x(0).
 * The message in x(1) ends up as the result of the send in x(0).
 */
public class Send {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        PID pid = proc.getX(0).toPID();
        Term message = proc.getX(1);
        Process destination = (Process) ProcessRegistry.resolve(pid);
        destination.send(message);
        proc.setX(0, message);
        return null;
    }

}
