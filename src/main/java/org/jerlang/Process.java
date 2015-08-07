package org.jerlang;

import java.util.Arrays;
import java.util.Objects;
import java.util.concurrent.LinkedBlockingQueue;

import org.jerlang.type.Fun;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
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

    // See "erts/emulator/beam/erl_vm.h":
    private final int MAX_REG = 1024;

    private final PID pid;
    private final LinkedBlockingQueue<Term> mailbox;
    private final ProcessDictionary dictionary;
    private ProcessPriority priority = ProcessPriority.NORMAL;
    private Scheduler scheduler = null;
    private Term[] registers = null;

    // Continuation Pointer
    private int cp = 0;

    // Stack / Y register
    private Term[] stack = new Term[10];
    private int sp = 0;

    public Process() {
        pid = new PID(1);
        dictionary = new ProcessDictionary();
        mailbox = new LinkedBlockingQueue<>();
    }

    public Process(Fun fun) {
        this();
    }

    public void allocate(int size, int keep) {
        Term[] newStack = new Term[stack.length + size];
        if (stack.length > 0) {
            System.arraycopy(stack, 0, newStack, 0, stack.length);
        }
        stack = newStack;
    }

    public void allocate_zero(int size, int keep) {
        allocate(size, keep);
        Arrays.fill(stack, stack.length - size, stack.length, List.nil);
        sp += size;
    }

    public void deallocate(int size) {
        Term[] newStack = new Term[stack.length - size];
        System.arraycopy(stack, 0, newStack, 0, newStack.length);
        stack = newStack;
        sp -= size;
    }

    public int getCP() {
        return cp;
    }

    public Term getX(int index) {
        return registers[index];
    }

    public Term getX(Integer index) {
        return getX(index.toInt());
    }

    public Term getY(int index) {
        return stack[sp - index];
    }

    public Term getY(Integer index) {
        return getY(index.toInt());
    }

    public Term popStack() {
        return stack[--sp];
    }

    public void pushStack(Term term) {
        stack[sp++] = term;
    }

    public void send(Term message) {
        try {
            mailbox.put(message);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

    public void setCP(int cp) {
        this.cp = cp;
    }

    public void setX(Integer index, Term term) {
        registers[index.toInt()] = term;
    }

    public void setY(Integer index, Term term) {
        stack[sp - index.toInt()] = term;
    }

    public void restoreCP() {
        cp = popStack().toInteger().toInt();
    }

    public void storeCP() {
        pushStack(Integer.of(cp));
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

    public Term[] registers() {
        if (registers == null) {
            registers = new Term[MAX_REG];
        }
        return registers;
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
