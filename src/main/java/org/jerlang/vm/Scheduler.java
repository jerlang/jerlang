package org.jerlang.vm;

import java.util.HashSet;

import org.jerlang.ProcessOrPort;
import org.jerlang.ProcessRegistry;
import org.jerlang.type.PidOrPortId;

/**
 * References:
 * http://www.erlang.se/euc/08/euc_smp.pdf
 */
public class Scheduler extends Thread {

    private final HashSet<PidOrPortId> ids;

    private final RunQueue<ProcessOrPort> runQueueMax;
    private final RunQueue<ProcessOrPort> runQueueHigh;
    private final RunQueue<ProcessOrPort> runQueueNormal;
    private final RunQueue<ProcessOrPort> runQueueLow;

    public Scheduler() {
        ids = new HashSet<>();
        runQueueMax = new RunQueue<>();
        runQueueHigh = new RunQueue<>();
        runQueueNormal = new RunQueue<>();
        runQueueLow = new RunQueue<>();
    }

    public ProcessOrPort add(ProcessOrPort p) {
        if (ids.contains(p.id())) {
            System.err.println("Process or port already scheduled");
            return p;
        }

        ids.add(p.id());

        p.setScheduler(this);
        switch (p.priority()) {
        case MAX:
            runQueueMax.push(p);
            break;
        case HIGH:
            runQueueHigh.push(p);
            break;
        case NORMAL:
            runQueueNormal.push(p);
            break;
        case LOW:
            runQueueLow.push(p);
            break;
        }
        return p;
    }

    /**
     * Performing actions according to:
     * https://www.erlang-solutions.com/resources/webinars/understanding-erlang-scheduler
     */
    @Override
    public void run() {
        while (!isInterrupted()) {
            checkIfTimersShouldBeTriggered();
            maybeCheckBalance();
            migrateProcessesAndPorts();
            executeAnyAuxWork();
            maybeCheckIO();
            executeOnePort();
            executeOneProcess();
        }
    }

    private void checkIfTimersShouldBeTriggered() {
    }

    private void maybeCheckBalance() {
    }

    private void migrateProcessesAndPorts() {
    }

    private void executeAnyAuxWork() {
    }

    private void maybeCheckIO() {
    }

    private void executeOnePort() {
    }

    private void executeOneProcess() {
        if (!execute(runQueueMax)) {
            if (!execute(runQueueHigh)) {
                if (!execute(runQueueNormal)) {
                    if (!execute(runQueueLow)) {
                        sleep();
                    }
                }
            }
        }
    }

    private boolean execute(RunQueue<ProcessOrPort> runQueue) {
        ProcessOrPort p = runQueue.poll();
        if (p != null) {
            ProcessRegistry.self(p);
            p.execute();
            ProcessRegistry.self(null);
            switch (p.state()) {
            case RUNNABLE:
                runQueue.push(p);
                break;
            default:
                ids.remove(p.pid());
                break;
            }
            return true;
        }
        return false;
    }

    private void sleep() {
        try {
            Thread.sleep(100);
        } catch (InterruptedException e) {
            interrupt();
        }
    }

}
