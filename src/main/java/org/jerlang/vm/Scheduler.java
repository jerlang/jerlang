package org.jerlang.vm;

import java.util.HashSet;

import org.jerlang.Process;
import org.jerlang.ProcessOrPort;
import org.jerlang.ProcessPriority;
import org.jerlang.ProcessRegistry;
import org.jerlang.ProcessSignals;
import org.jerlang.ProcessState;
import org.jerlang.type.Atom;
import org.jerlang.type.PidOrPortId;
import org.jerlang.type.Term;

/**
 * References:
 * http://www.erlang.se/euc/08/euc_smp.pdf
 */
public class Scheduler extends Thread {

    private static final boolean DEBUG_EXITS = Boolean.getBoolean("jerlang.debug.errors");

    /**
     * Instructions a process may execute before it is put back on the run
     * queue. One instruction is one reduction; the number is deliberately
     * small enough that a CPU-bound process cannot hide a waking process for
     * tens of milliseconds, and large enough that the queue traffic stays
     * low.
     */
    private static final long REDUCTIONS_PER_SLICE = 1000;

    private final HashSet<PidOrPortId> ids;

    private final RunQueue<ProcessOrPort> runQueueMax;
    private final RunQueue<ProcessOrPort> runQueueHigh;
    private final RunQueue<ProcessOrPort> runQueueNormal;
    private final RunQueue<ProcessOrPort> runQueueLow;

    /** Guards {@link #workPending}, which carries a wakeup across the gap
     * between finding the queues empty and starting to wait. */
    private final Object workLock = new Object();
    private boolean workPending = false;

    /**
     * Number of blocking calls (BIF waits that poll instead of parking the
     * process) currently running on this scheduler's thread. New processes
     * must not be queued here while it is greater than zero: the thread can
     * not drain its run queue until the call returns, and the call is often
     * waiting for exactly those new processes.
     */
    private int blockingCalls = 0;

    /**
     * Time this scheduler spent executing work, and parked because there was
     * none. Written only by the scheduler thread and read by a metrics
     * scrape, so it is a plain volatile pair rather than an atomic on the
     * loop.
     */
    private volatile long busyNanos = 0;
    private volatile long idleNanos = 0;

    public Scheduler() {
        ids = new HashSet<>();
        runQueueMax = new RunQueue<>();
        runQueueHigh = new RunQueue<>();
        runQueueNormal = new RunQueue<>();
        runQueueLow = new RunQueue<>();
    }

    public synchronized void enterBlockingCall() {
        blockingCalls++;
    }

    public synchronized void exitBlockingCall() {
        if (blockingCalls > 0) {
            blockingCalls--;
        }
    }

    public synchronized boolean isInBlockingCall() {
        return blockingCalls > 0;
    }

    public ProcessOrPort add(ProcessOrPort p) {
        // A waiting process keeps its id while it is off the run queue, so
        // membership must not prevent a message or timeout from re-queueing
        // it. Only RUNNABLE processes are ever pushed back.
        ids.add(p.id());

        p.setScheduler(this);
        queue(p.priority()).push(p);
        // Signal only after the process is on a queue, so a woken scheduler
        // can not poll an empty queue and park again while work sits unseen.
        wakeup();
        return p;
    }

    /**
     * Move a process that is still queued to the queue for its new priority.
     * A process whose `process_flag(priority, ...)` call is running has
     * already been polled, and `remove` returns false: it finishes the slice
     * and is pushed to the new queue by the scheduler loop.
     */
    public void reprioritize(ProcessOrPort p, ProcessPriority previous) {
        if (previous == p.priority()) {
            return;
        }
        if (queue(previous).remove(p)) {
            queue(p.priority()).push(p);
            wakeup();
        }
    }

    private RunQueue<ProcessOrPort> queue(ProcessPriority priority) {
        switch (priority) {
        case MAX:
            return runQueueMax;
        case HIGH:
            return runQueueHigh;
        case NORMAL:
            return runQueueNormal;
        case LOW:
        default:
            return runQueueLow;
        }
    }

    /**
     * Performing actions according to:
     * https://www.erlang-solutions.com/resources/webinars/understanding-erlang-scheduler
     */
    @Override
    public void run() {
        while (!isInterrupted()) {
            long started = System.nanoTime();
            boolean worked = executeOneProcess();
            long elapsed = System.nanoTime() - started;
            if (worked) {
                busyNanos += elapsed;
            } else {
                idleNanos += elapsed;
            }
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

    /**
     * Run one process, or park when there is none. Returns whether work was
     * executed, so the loop can account for the time it just spent.
     */
    private boolean executeOneProcess() {
        checkIfTimersShouldBeTriggered();
        maybeCheckBalance();
        migrateProcessesAndPorts();
        executeAnyAuxWork();
        maybeCheckIO();
        executeOnePort();
        if (execute(runQueueMax)) {
            return true;
        }
        if (execute(runQueueHigh)) {
            return true;
        }
        if (execute(runQueueNormal)) {
            return true;
        }
        if (execute(runQueueLow)) {
            return true;
        }
        sleep();
        return false;
    }

    /** Processes and ports waiting on this scheduler, across all priorities. */
    public int queueDepth() {
        return runQueueMax.size() + runQueueHigh.size()
            + runQueueNormal.size() + runQueueLow.size();
    }

    /** Nanoseconds this scheduler spent executing work. */
    public long busyNanos() {
        return busyNanos;
    }

    /** Nanoseconds this scheduler spent parked with an empty queue. */
    public long idleNanos() {
        return idleNanos;
    }

    private boolean execute(RunQueue<ProcessOrPort> runQueue) {
        ProcessOrPort p = runQueue.poll();
        if (p != null) {
            ProcessRegistry.self(p);
            Process process = p instanceof Process ? (Process) p : null;
            Throwable failure = null;
            try {
                // An exit signal that arrived while the process was off the
                // run queue takes effect before another instruction runs.
                if (process != null && process.hasPendingExit()
                    && p.state() != ProcessState.EXITING) {
                    p.setState(ProcessState.EXITING);
                }
                if (p.state() != ProcessState.EXITING) {
                    if (process != null) {
                        process.beginTimeSlice(REDUCTIONS_PER_SLICE);
                    }
                    p.execute();
                }
            } catch (Throwable t) {
                // An uncaught error terminates the process, but must not
                // terminate the scheduler thread that ran it.
                failure = t;
            } finally {
                ProcessRegistry.self(null);
            }
            if (failure != null) {
                if (process != null) {
                    process.setFailure(failure);
                }
                p.setState(ProcessState.EXITING);
            }
            if (process != null && p.state() == ProcessState.EXITING) {
                // An uncaught exception is reported before the links and
                // monitors hear about it, so the report is not lost when a
                // linked process handles the exit first.
                CrashReport.print(process);
                // The exit reason is the pending signal when there is one,
                // otherwise what the process crashed with or `normal`.
                Term reason = process.pendingExitReason();
                if (reason == null) {
                    reason = process.failure() == null
                        ? Atom.of("normal")
                        : ProcessSignals.reason(process.failure());
                }
                if (DEBUG_EXITS) {
                    System.err.println("EXIT " + process.pid() + " reason=" + reason
                        + " failure=" + process.failure()
                        + " name=" + ProcessRegistry.registeredName(process.pid()));
                }
                ProcessSignals.terminate(process, reason);
            }
            if (p.state() == ProcessState.EXITING) {
                // Nothing more will run for this process, so a caller waiting
                // on it can read the result now. This has to be here rather
                // than on the state change: EXITING is also reached inside a
                // nested call, before the outer activation has finished.
                p.markTerminated();
            }
            switch (p.state()) {
            case RUNNABLE:
                runQueue.push(p);
                break;
            case WAITING:
                // Keep it known to this scheduler. Process.send or its
                // timer will make it runnable and enqueue it again.
                break;
            default:
                ids.remove(p.pid());
                break;
            }
            return true;
        }
        return false;
    }

    /**
     * Wake a scheduler that is waiting for work. Called for every enqueue, so
     * a newly runnable process does not wait for the next poll.
     */
    private void wakeup() {
        synchronized (workLock) {
            workPending = true;
            workLock.notify();
        }
    }

    /**
     * Park until {@link #add} signals work.
     *
     * This used to sleep for 100 ms, which put a floor under every call made
     * from outside the VM: the spawned process waited for the next poll of an
     * idle scheduler. Waiting on the signal costs nothing when work arrives
     * promptly and still parks the thread when it does not.
     */
    private void sleep() {
        synchronized (workLock) {
            if (workPending) {
                workPending = false;
                return;
            }
            try {
                workLock.wait();
            } catch (InterruptedException e) {
                interrupt();
            }
            workPending = false;
        }
    }

}
