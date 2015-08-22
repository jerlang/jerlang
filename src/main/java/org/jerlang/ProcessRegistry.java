package org.jerlang;

import java.util.HashMap;
import java.util.Map;

import org.jerlang.type.Atom;
import org.jerlang.type.PID;
import org.jerlang.vm.VirtualMachine;

public class ProcessRegistry {

    private static final ProcessRegistry instance = new ProcessRegistry();
    private final Map<Atom, PID> processes;
    private final Map<PID, Process> pid2process;

    public ProcessRegistry() {
        processes = new HashMap<>();
        pid2process = new HashMap<>();
    }

    public PID get(Atom name) {
        return processes.get(name);
    }

    public static ProcessRegistry instance() {
        return instance;
    }

    public static void register(Atom name, PID pid) {
        instance.processes.put(name, pid);
    }

    public static void register(Process process) {
        instance.pid2process.put(process.pid(), process);
    }

    public static Process resolve(PID pid) {
        return instance.pid2process.get(pid);
    }

    public static Process self() {
        return VirtualMachine.instance().self();
    }

}
