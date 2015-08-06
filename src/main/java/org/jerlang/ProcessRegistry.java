package org.jerlang;

import java.util.HashMap;
import java.util.Map;

import org.jerlang.type.Atom;
import org.jerlang.type.PID;

public class ProcessRegistry {

    private static final ProcessRegistry instance = new ProcessRegistry();
    private final Map<Atom, PID> processes;

    public ProcessRegistry() {
        processes = new HashMap<>();
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

    public static Process self() {
        return new Process(); // TODO
    }

}
