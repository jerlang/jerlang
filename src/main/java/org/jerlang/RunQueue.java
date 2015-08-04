package org.jerlang;

import java.util.concurrent.ConcurrentLinkedDeque;

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

}
