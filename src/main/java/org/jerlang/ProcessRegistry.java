package org.jerlang;

import java.util.HashMap;
import java.util.Map;

import org.jerlang.type.Atom;
import org.jerlang.type.PidOrPortId;

public class ProcessRegistry {

    private static final ProcessRegistry instance = new ProcessRegistry();
    private final Map<Atom, PidOrPortId> processes;
    private final Map<PidOrPortId, ProcessOrPort> id2process;

    private ThreadLocal<ProcessOrPort> process = new ThreadLocal<>();

    public ProcessRegistry() {
        processes = new HashMap<>();
        id2process = new HashMap<>();
    }

    public void cleanup() {
        processes.clear();
        id2process.clear();
        process = new ThreadLocal<>();
    }

    public PidOrPortId get(Atom name) {
        return processes.get(name);
    }

    public static ProcessRegistry instance() {
        return instance;
    }

    public static void register(Atom name, PidOrPortId id) {
        instance.processes.put(name, id);
    }

    public static void register(Process process) {
        instance.id2process.put(process.id(), process);
    }

    public static ProcessOrPort resolve(PidOrPortId id) {
        return instance.id2process.get(id);
    }

    public static ProcessOrPort self() {
        return instance.process.get();
    }

    public static void self(ProcessOrPort process) {
        instance.process.set(process);
    }

}
