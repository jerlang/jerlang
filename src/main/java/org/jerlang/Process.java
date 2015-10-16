package org.jerlang;

import java.util.Arrays;
import java.util.Stack;
import java.util.concurrent.LinkedBlockingQueue;

import org.jerlang.type.Atom;
import org.jerlang.type.BitString;
import org.jerlang.type.Float;
import org.jerlang.type.Fun;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.PID;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;
import org.jerlang.type.stack.ExceptionHandler;

/**
 * Erlang is designed for massive concurrency.
 * Erlang processes are lightweight (grow and shrink dynamically)
 * with small memory footprint, fast to create and terminate,
 * and the scheduling overhead is low.
 *
 * Source:
 * http://www.erlang.org/doc/reference_manual/processes.html
 */
public class Process extends ProcessOrPort {

    // See "erts/emulator/beam/erl_vm.h":
    private final int MAX_REG = 1024;
    private final int MAX_FREG = 32;

    private final LinkedBlockingQueue<Term> mailbox;
    private final ProcessDictionary dictionary;
    private Term[] registers = null;
    private Float[] fregisters = new Float[MAX_FREG];

    // Continuation Pointer
    private int cp = -1;
    private Stack<java.lang.Integer> cpStack = new Stack<>();

    // Stack / Y register
    private Term[] stack = new Term[16];
    private int sp = 0;

    // Signatures
    // We need a stack for the interpreter to find the correct labels
    private Stack<FunctionSignature> signatures = new Stack<>();

    // Used by `put_tuple/2` and `put/1`:
    private Tuple tuple;
    private int tupleIndex;

    // BitString, used by `bs_put_integer/5`
    private BitString bitString;

    // Exception handling
    private ExceptionHandler exceptionHandler = null;

    // Timeout
    private long timeout = 0;

    public Process(PID pid) {
        super(pid);
        dictionary = new ProcessDictionary();
        mailbox = new LinkedBlockingQueue<>();
    }

    public Process(PID pid, Fun fun) {
        this(pid);
    }

    public Process(PID pid, Atom module, Atom fun, List args) {
        this(pid);

        FunctionSignature s = new FunctionSignature(module, fun, Integer.of(args.length()));
        signatures.push(s);

        int index = 0;
        while (args.length() > 0) {
            setX(index++, args.head());
            args = args.tail();
        }
    }

    public void allocate(int size, int keep) {
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

    public BitString bitString() {
        return bitString;
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
        return registers()[index];
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
            mailbox.put(message);
            if (state() == ProcessState.WAITING) {
                setState(ProcessState.RUNNABLE);
                scheduler().add(this);
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

    public void setTuple(Tuple tuple) {
        this.tuple = tuple;
        this.tupleIndex = 0;
    }

    public void setX(int index, Term term) {
        // System.err.println("" + this + ": X" + index + " = " + term);
        registers()[index] = term;
    }

    public void setX(Integer index, Term term) {
        setX(index.toInt(), term);
    }

    public void setY(Integer index, Term term) {
        int pos = sp - index.toInt();
        stack[sp - index.toInt()] = term;
    }

    public FunctionSignature signature() {
        return signatures.peek();
    }

    public void signature(FunctionSignature signature) {
        signatures.pop();
        signatures.push(signature);
    }

    public void restoreCP() {
        cp = cpStack.pop();
    }

    public void storeCP() {
        cpStack.push(cp);
    }

    public ExceptionHandler exceptionHandler() {
        return exceptionHandler;
    }

    @Override
    public void execute() {
        switch (state()) {
        case WAITING:
            if (hasMessage()) {
                setState(ProcessState.RUNNING);
                Interpreter.continueExecution(signatures.peek(), cp);
            }
            break;
        case RUNNABLE:
            Interpreter.continueExecution(signatures.peek(), cp);
            break;
        default:
            System.out.println("Skipping process " + pid() + " in state " + state());
            break;
        }
    }

    public void loop_rec_end() {
        // TODO: this is technically not correct
        // TODO: we need to advance the message index instead
        mailbox.remove();
    }

    public Term[] registers() {
        if (registers == null) {
            registers = new Term[MAX_REG];
        }
        return registers;
    }

    public void setBitString(BitString bitString) {
        this.bitString = bitString;
    }

    public void setExceptionHandler(ExceptionHandler exceptionHandler) {
        this.exceptionHandler = exceptionHandler;
    }

    // TODO: Only for debugging, should be removed before 1.0
    public void printStack() {
        System.out.print("REG: [" + registers()[0] + ", ");
        System.out.print("" + registers()[1] + ", ");
        System.out.print("" + registers()[2] + ", ");
        System.out.print("" + registers()[3] + "] ");

        System.out.print("STACK(" + stack.length + "): ");
        for (Term t : stack) {
            System.out.print("" + t + ", ");
        }
        System.out.println();
    }

    public void removeMessage() {
        Term message = mailbox.remove();
        setX(0, message);
        // TODO: remove timer
    }

    public void popSignature() {
        signatures.pop();
    }

    public void pushSignature(FunctionSignature signature) {
        signatures.push(signature);
    }

    public void clearTimeout() {
        timeout = 0;
    }

    public void setTimeout() {
        timeout = System.currentTimeMillis();
    }

    @Override
    public String toString() {
        return pid().toString();
    }

    public void resetMailbox() {
        // TODO
    }

}
