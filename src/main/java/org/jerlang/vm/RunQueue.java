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

}
