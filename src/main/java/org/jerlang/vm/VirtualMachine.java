package org.jerlang.vm;

import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import org.jerlang.Process;
import org.jerlang.ProcessRegistry;
import org.jerlang.type.Atom;
import org.jerlang.type.Fun;
import org.jerlang.type.List;
import org.jerlang.type.PID;
import org.jerlang.type.Term;
import org.jerlang.type.TimerReference;
import org.jerlang.type.Tuple;

/**
 *
 */
public class VirtualMachine {

    private Scheduler[] schedulers;
    private volatile int nextScheduler = 0;

    private static final AtomicInteger nextPID = new AtomicInteger();

    private ScheduledThreadPoolExecutor timerExecutor;

    private static final VirtualMachine VIRTUAL_MACHINE = new VirtualMachine();

    private VirtualMachine() {
        int processors = Runtime.getRuntime().availableProcessors();
        schedulers = new Scheduler[processors];
        for (int index = 0; index < processors; index++) {
            schedulers[index] = new Scheduler();
        }
        timerExecutor = new ScheduledThreadPoolExecutor(processors);
    }

    public Term cancel(TimerReference timerReference) {
        timerReference.future().cancel(false);
        return Tuple.of(Atom.of("ok"), Atom.of("cancel"));
    }

    public static Process self() {
        return ProcessRegistry.self();
    }

    public void start() {
        int processors = Runtime.getRuntime().availableProcessors();
        schedulers = new Scheduler[processors];
        for (int index = 0; index < processors; index++) {
            schedulers[index] = new Scheduler();
        }
        for (Scheduler scheduler : schedulers) {
            scheduler.start();
        }
        timerExecutor = new ScheduledThreadPoolExecutor(processors);
        ProcessRegistry.instance().cleanup();
    }

    public void shutdown() {
        for (Scheduler scheduler : schedulers) {
            scheduler.interrupt();
        }
        for (Scheduler scheduler : schedulers) {
            try {
                scheduler.join();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
        ProcessRegistry.instance().cleanup();
    }

    public static VirtualMachine instance() {
        return VIRTUAL_MACHINE;
    }

    public Process resolve(PID pid) {
        return ProcessRegistry.resolve(pid);
    }

    public TimerReference send_after(int time, Term message, PID pid) {
        return new TimerReference(timerExecutor.schedule(
            new Runnable() {

                @Override
                public void run() {
                    System.out.println("VM.send_after: " + message + " TO " + pid);
                    resolve(pid).send(message);
                }

            }, time, TimeUnit.MILLISECONDS));
    }

    public Process spawn(Fun fun) {
        PID pid = new PID(nextPID.incrementAndGet());
        Process process = new Process(pid, fun);
        ProcessRegistry.register(process);
        System.out.println("Spawned1 " + process);
        // Round-robin process assignment
        return schedulers[nextScheduler++ % schedulers.length].add(process);
    }

    public Process spawn(Atom module, Atom fun, List args) {
        PID pid = new PID(nextPID.incrementAndGet());
        Process process = new Process(pid, module, fun, args);
        ProcessRegistry.register(process);
        System.out.println("Spawned2 " + process + ": " + module + ":" + fun);
        // Round-robin process assignment
        return schedulers[nextScheduler++ % schedulers.length].add(process);
    }

}
