package org.jerlang.vm;

import java.util.concurrent.ConcurrentLinkedDeque;

import org.jerlang.ProcessOrPort;

public class RunQueue<T extends ProcessOrPort> {

    private final ConcurrentLinkedDeque<T> queue;

    public RunQueue() {
        queue = new ConcurrentLinkedDeque<>();
    }

    public T poll() {
        return queue.poll();
    }

    public void push(T processOrPort) {
        queue.add(processOrPort);
    }

    /**
     * Number of processes and ports waiting in this queue, for the metrics
     * snapshot. Counting is a traversal, so this is a scrape-time cost and
     * never part of the scheduler loop.
     */
    public int size() {
        return queue.size();
    }

    /**
     * Remove a queued process or port, if it is still in this queue. A
     * process that was already polled is executing and must not be moved.
     */
    public boolean remove(T processOrPort) {
        return queue.remove(processOrPort);
    }

}
