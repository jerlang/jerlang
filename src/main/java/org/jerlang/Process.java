package org.jerlang;

import java.util.Arrays;
import java.util.Objects;
import java.util.concurrent.LinkedBlockingQueue;

import org.jerlang.type.Atom;
import org.jerlang.type.Float;
import org.jerlang.type.Fun;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.PID;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;
import org.jerlang.type.stack.ExceptionHandler;
import org.jerlang.vm.Scheduler;

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

    private ProcessState state = ProcessState.RUNNABLE;

    // See "erts/emulator/beam/erl_vm.h":
    private final int MAX_REG = 1024;
    private final int MAX_FREG = 32;

    private final PID pid;
    private final LinkedBlockingQueue<Term> mailbox;
    private final ProcessDictionary dictionary;
    private ProcessPriority priority = ProcessPriority.NORMAL;
    private Scheduler scheduler = null;
    private Term[] registers = null;
    private Float[] fregisters = new Float[MAX_FREG];

    // Continuation Pointer
    private int cp = -1;

    // Stack / Y register
    private Term[] stack = new Term[16];
    private int sp = 0;

    private FunctionSignature signature; // TODO: make final

    // Used by `put_tuple/2` and `put/1`:
    private Tuple tuple;
    private int tupleIndex;

    // Exception handling
    private ExceptionHandler exceptionHandler = null;

    public Process(PID pid) {
        this.pid = pid;
        dictionary = new ProcessDictionary();
        mailbox = new LinkedBlockingQueue<>();
    }

    public Process(PID pid, Fun fun) {
        this(pid);
    }

    public Process(PID pid, Atom module, Atom fun, List args) {
        this(pid);
        signature = new FunctionSignature(module, fun, Integer.of(args.length()));
    }

    public void allocate(int size, int keep) {
        System.out.println("Allocate " + size + ", now " + (stack.length + size));
        Term[] newStack = new Term[stack.length + size];
        if (stack.length > 0) {
            System.arraycopy(stack, 0, newStack, 0, stack.length);
        }
        stack = newStack;
        sp += size;
    }

    public void allocate_heap(int stack, int heap, int live) {
        if (stack > 0) {
            allocate(stack, live);
        }
    }

    public void allocate_heap_zero(int stack, int heap, int live) {
        if (stack > 0) {
            allocate_zero(stack, live);
        }
    }

    public void allocate_zero(int size, int keep) {
        if (size > 0) {
            allocate(size, keep);
            Arrays.fill(stack, stack.length - size, stack.length, List.nil);
        }
    }

    public void deallocate(int size) {
        if (size > 0) {
            Term[] newStack = new Term[stack.length - size];
            System.arraycopy(stack, 0, newStack, 0, newStack.length);
            stack = newStack;
            sp -= size;
        }
    }

    public ProcessDictionary dictionary() {
        return dictionary;
    }

    public boolean hasMessage() {
        return !mailbox.isEmpty();
    }

    public int getCP() {
        return cp;
    }

    public Float getFR(int index) {
        return fregisters[index];
    }

    public Term nextMessage() {
        return mailbox.peek();
    }

    public Tuple getTuple() {
        return tuple;
    }

    public int getTupleIndex() {
        return tupleIndex;
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

    public void incrementTupleIndex() {
        tupleIndex++;
    }

    public Term popStack() {
        return stack[--sp];
    }

    public void pushStack(Term term) {
        stack[sp++] = term;
    }

    public void send(Term message) {
        try {
            System.out.println("Put MSG '" + message + "' to " + this + "'s inbox");
            mailbox.put(message);
            if (state == ProcessState.WAITING) {
                state = ProcessState.RUNNABLE;
                scheduler.add(this);
            }
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

    public void setCP(int cp) {
        this.cp = cp;
    }

    public void setFR(Integer index, Float value) {
        fregisters[index.toInt()] = value;
    }

    public void setState(ProcessState state) {
        this.state = state;
    }

    public void setTuple(Tuple tuple) {
        this.tuple = tuple;
        this.tupleIndex = 0;
    }

    public void setX(int index, Term term) {
        registers()[index] = term;
    }

    public void setX(Integer index, Term term) {
        setX(index.toInt(), term);
    }

    public void setY(Integer index, Term term) {
        int pos = sp - index.toInt();
        if (pos < 0 || pos >= stack.length) {
            System.err.println("setY(" + pos + "," + term + ") failed: " + stack.length);
        }
        stack[sp - index.toInt()] = term;
    }

    public FunctionSignature signature() {
        return signature;
    }

    public ProcessState state() {
        return state;
    }

    public void restoreCP() {
        cp = popStack().toInteger().toInt();
    }

    public void storeCP() {
        pushStack(Integer.of(cp));
    }

    public ExceptionHandler exceptionHandler() {
        return exceptionHandler;
    }

    public void execute() {
        switch (state) {
        case WAITING:
            if (state == ProcessState.WAITING && hasMessage()) {
                System.out.println("Executing process " + pid + " in state " + state);
                state = ProcessState.RUNNING;
                Term result = Interpreter.continueExecution(signature.module(), cp);
                System.out.println("RESULT: " + result);
            }
            break;
        case RUNNABLE:
            System.out.println("Executing process " + pid + " in state " + state);
            Term result = Interpreter.continueExecution(signature.module(), cp);
            System.out.println("RESULT: " + result);
            break;
        default:
            System.out.println("Skipping process " + pid + " in state " + state);
            break;
        }
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

    public void setExceptionHandler(ExceptionHandler exceptionHandler) {
        this.exceptionHandler = exceptionHandler;
    }

    public void setPriority(ProcessPriority priority) {
        this.priority = Objects.requireNonNull(priority);
    }

    public void setScheduler(Scheduler scheduler) {
        this.scheduler = scheduler;
    }

    // TODO: Only for debugging, should be removed before 1.0
    public void printStack() {
        System.out.print("REG: [" + registers()[0] + ", ");
        System.out.print("" + registers()[1] + ", ");
        System.out.print("" + registers()[2] + "] ");
        System.out.print("STACK: ");
        for (Term t : stack) {
            System.out.print("" + t + ", ");
        }
        System.out.println();
    }

    @Override
    public String toString() {
        return pid.toString();
    }

    public void removeMessage() {
        Term message = mailbox.remove();
        setX(Integer.of(0), message);
        // TODO: remove timer
    }

}
