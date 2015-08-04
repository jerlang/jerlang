package org.jerlang;

import java.util.Objects;
import java.util.concurrent.LinkedBlockingQueue;

import org.jerlang.type.Fun;
import org.jerlang.type.PID;
import org.jerlang.type.Term;

/**
 * Erlang is designed for massive concurrency.
 * Erlang processes are lightweight (grow and shrink dynamically)
 * with small memory footprint, fast to create and terminate,
 * and the scheduling overhead is low.
 *
 * Source:
 * http://www.erlang.org/doc/reference_manual/processes.html
 */
public class Process implements ProcessOrPort {

    private final PID pid;
    private final LinkedBlockingQueue<Term> mailbox;
    private final ProcessDictionary dictionary;
    private ProcessPriority priority = ProcessPriority.NORMAL;
    private Scheduler scheduler = null;

    public Process() {
        pid = new PID(1);
        dictionary = new ProcessDictionary();
        mailbox = new LinkedBlockingQueue<>();
    }

    public Process(Fun fun) {
        this();
    }

    public void send(Term message) {
        try {
            mailbox.put(message);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

    public ProcessDictionary dictionary() {
        return dictionary;
    }

    public void execute() {
    }

    public PID pid() {
        return pid;
    }

    public ProcessPriority priority() {
        return priority;
    }

    public Scheduler scheduler() {
        return scheduler;
    }

    public void setPriority(ProcessPriority priority) {
        this.priority = Objects.requireNonNull(priority);
    }

    public void setScheduler(Scheduler scheduler) {
        this.scheduler = scheduler;
    }

}
