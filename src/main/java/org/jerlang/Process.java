package org.jerlang;

import java.util.concurrent.LinkedBlockingQueue;

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
public class Process {

    private final PID pid;
    private final LinkedBlockingQueue<Term> mailbox;
    private final ProcessDictionary dictionary;

    public Process() {
        pid = new PID(1);
        dictionary = new ProcessDictionary();
        mailbox = new LinkedBlockingQueue<>();
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

    public PID pid() {
        return pid;
    }

}
