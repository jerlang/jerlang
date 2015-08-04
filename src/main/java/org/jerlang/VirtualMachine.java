package org.jerlang;

import org.jerlang.type.PID;

public class VirtualMachine {

    private Scheduler[] schedulers;
    private volatile int nextScheduler = 0;

    private static final VirtualMachine VIRTUAL_MACHINE = new VirtualMachine();

    private VirtualMachine() {
        int processors = Runtime.getRuntime().availableProcessors();
        schedulers = new Scheduler[processors];
        for (int index = 0; index < processors; index++) {
            schedulers[index] = new Scheduler();
        }
    }

    public static VirtualMachine instance() {
        return VIRTUAL_MACHINE;
    }

    public PID spawn(Process process) {
        // Round-robin process assignment
        return schedulers[nextScheduler++ % schedulers.length].add(process);
    }

}
