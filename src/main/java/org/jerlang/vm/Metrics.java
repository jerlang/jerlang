package org.jerlang.vm;

import java.util.Locale;

import org.jerlang.ProcessRegistry;

/**
 * One reading of the runtime counters a service is monitored by: how many
 * processes and ports exist, how much work is queued, how many reductions the
 * live processes have executed, how many messages are sitting in mailboxes,
 * and how busy each scheduler is.
 *
 * A snapshot is a sample, not a stream: the numbers are read one process at a
 * time, so a scrape that overlaps with processes starting and dying can be
 * slightly inconsistent. That is the same trade Erlang/OTP makes for
 * {@code erlang:statistics/1}, and it keeps the numbers free: nothing on the
 * hot path increments a counter for this.
 *
 * Reductions are summed over the processes that are alive at the reading, so
 * the value is not cumulative for the node's lifetime. It answers "how much
 * work is this node doing now", not "how much work has it done".
 */
public final class Metrics {

    private final long processes;
    private final long ports;
    private final long reductions;
    private final long mailboxes;
    private final long runQueueDepth;
    private final long uptimeMillis;
    private final long[] schedulerQueueDepth;
    private final long[] schedulerBusyNanos;
    private final long[] schedulerIdleNanos;

    private Metrics(long processes, long ports, long reductions, long mailboxes,
        long runQueueDepth, long uptimeMillis, long[] schedulerQueueDepth,
        long[] schedulerBusyNanos, long[] schedulerIdleNanos) {
        this.processes = processes;
        this.ports = ports;
        this.reductions = reductions;
        this.mailboxes = mailboxes;
        this.runQueueDepth = runQueueDepth;
        this.uptimeMillis = uptimeMillis;
        this.schedulerQueueDepth = schedulerQueueDepth;
        this.schedulerBusyNanos = schedulerBusyNanos;
        this.schedulerIdleNanos = schedulerIdleNanos;
    }

    /**
     * Read the counters of the running virtual machine.
     */
    public static Metrics snapshot() {
        VirtualMachine virtualMachine = VirtualMachine.instance();
        Scheduler[] schedulers = virtualMachine.schedulers();
        int count = schedulers.length;
        long[] queues = new long[count];
        long[] busy = new long[count];
        long[] idle = new long[count];
        long queued = 0;
        for (int index = 0; index < count; index++) {
            queues[index] = schedulers[index].queueDepth();
            busy[index] = schedulers[index].busyNanos();
            idle[index] = schedulers[index].idleNanos();
            queued += queues[index];
        }
        long started = virtualMachine.startedAtMillis();
        long uptime = started == 0 ? 0 : Math.max(0, System.currentTimeMillis() - started);
        return new Metrics(
            ProcessRegistry.countProcesses(),
            ProcessRegistry.countPorts(),
            ProcessRegistry.totalReductions(),
            ProcessRegistry.totalMailboxSize(),
            queued,
            uptime,
            queues,
            busy,
            idle);
    }

    /** Live processes, not including ports. */
    public long processes() {
        return processes;
    }

    /** Live ports. */
    public long ports() {
        return ports;
    }

    /** Reductions executed by the live processes so far. */
    public long reductions() {
        return reductions;
    }

    /** Messages waiting in all mailboxes together. */
    public long mailboxes() {
        return mailboxes;
    }

    /** Processes and ports waiting on all schedulers together. */
    public long runQueueDepth() {
        return runQueueDepth;
    }

    /** Milliseconds since this virtual machine was started, or 0 when it is not running. */
    public long uptimeMillis() {
        return uptimeMillis;
    }

    public int schedulerCount() {
        return schedulerQueueDepth.length;
    }

    /** Queue depth of one scheduler. */
    public long runQueueDepth(int scheduler) {
        return schedulerQueueDepth[scheduler];
    }

    /**
     * Fraction of one scheduler's time that was spent executing work rather
     * than parked with an empty queue, between 0.0 and 1.0.
     */
    public double utilisation(int scheduler) {
        long busy = schedulerBusyNanos[scheduler];
        long idle = schedulerIdleNanos[scheduler];
        long total = busy + idle;
        return total == 0 ? 0.0 : (double) busy / (double) total;
    }

    /** Utilisation of all schedulers together, between 0.0 and 1.0. */
    public double utilisation() {
        long busy = 0;
        long idle = 0;
        for (int index = 0; index < schedulerBusyNanos.length; index++) {
            busy += schedulerBusyNanos[index];
            idle += schedulerIdleNanos[index];
        }
        long total = busy + idle;
        return total == 0 ? 0.0 : (double) busy / (double) total;
    }

    /**
     * The reading in the documented line format: one metric per line, a name
     * and a value separated by a space. Per-scheduler lines carry the
     * scheduler index as a label. See docs/operations.md for the fields and
     * their units.
     */
    public String toText() {
        StringBuilder text = new StringBuilder();
        line(text, "jerlang_processes", Long.toString(processes));
        line(text, "jerlang_ports", Long.toString(ports));
        line(text, "jerlang_run_queue", Long.toString(runQueueDepth));
        line(text, "jerlang_reductions", Long.toString(reductions));
        line(text, "jerlang_mailbox_messages", Long.toString(mailboxes));
        line(text, "jerlang_schedulers", Integer.toString(schedulerCount()));
        line(text, "jerlang_uptime_millis", Long.toString(uptimeMillis));
        line(text, "jerlang_scheduler_utilisation", ratio(utilisation()));
        for (int index = 0; index < schedulerCount(); index++) {
            line(text, "jerlang_scheduler_queue{index=\"" + index + "\"}",
                Long.toString(schedulerQueueDepth[index]));
            line(text, "jerlang_scheduler_utilisation{index=\"" + index + "\"}",
                ratio(utilisation(index)));
        }
        return text.toString();
    }

    @Override
    public String toString() {
        return "Metrics[processes=" + processes
            + ", ports=" + ports
            + ", runQueueDepth=" + runQueueDepth
            + ", reductions=" + reductions
            + ", mailboxMessages=" + mailboxes
            + ", schedulers=" + schedulerCount()
            + ", utilisation=" + ratio(utilisation())
            + ", uptimeMillis=" + uptimeMillis
            + "]";
    }

    private static String ratio(double value) {
        return String.format(Locale.ROOT, "%.4f", value);
    }

    private static void line(StringBuilder text, String name, String value) {
        text.append(name).append(' ').append(value).append('\n');
    }

}
