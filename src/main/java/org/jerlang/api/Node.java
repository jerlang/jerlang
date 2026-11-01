package org.jerlang.api;

import java.util.function.Function;

import org.jerlang.Module;
import org.jerlang.ModuleRegistry;
import org.jerlang.jvm.HandleTable;
import org.jerlang.stdlib.Java;
import org.jerlang.type.Atom;
import org.jerlang.type.Fun;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.jerlang.vm.Metrics;
import org.jerlang.vm.VirtualMachine;

/**
 * = Embedding a node
 *
 * A node owns the runtime for as long as it is open: it starts the schedulers,
 * applies the options, registers the java module when reflection was asked
 * for, and on {@link #close()} drops that module and drains the object handle
 * table.
 *
 * ----
 * Node node = Node.start(NodeOptions.builder().reflection(true).build());
 * node.registerFunction("my_mod", "echo", 1, args -> args[0]);
 * Term reply = node.apply("gen_server", "call", pid, request, Integer.of(5000));
 * node.close();
 * ----
 *
 * {@code apply} takes both a module name and an Erlang fun.
 * {@code registerFunction} makes a Java lambda callable from Erlang through
 * the normal module registry. {@link Bridge} stays as it is: a static
 * convenience over a default node.
 */
public final class Node implements AutoCloseable {

    private final NodeOptions options;
    private boolean closed;

    private Node(NodeOptions options) {
        this.options = options;
    }

    public static Node start() {
        return start(NodeOptions.defaults());
    }

    public static Node start(NodeOptions options) {
        if (options == null) {
            throw new IllegalArgumentException("options must not be null");
        }
        VirtualMachine virtualMachine = VirtualMachine.instance();
        if (options.schedulers() > 0) {
            virtualMachine.start(options.schedulers());
        } else {
            virtualMachine.start();
        }
        virtualMachine.handleTable().setMaxHandles(options.maxHandles());
        Node node = new Node(options);
        if (options.reflection()) {
            node.registerJavaModule();
        }
        return node;
    }

    /**
     * Register the java module on this node. Without it there is no
     * java:new/2, so Erlang code cannot reach an arbitrary JVM class.
     */
    private void registerJavaModule() {
        Module module = new Module(Java.class, Atom.of("java"));
        module.export();
        ModuleRegistry.register(module);
    }

    public NodeOptions options() {
        return options;
    }

    public boolean hasReflection() {
        return options.reflection();
    }

    public boolean isClosed() {
        return closed;
    }

    /**
     * The name of the node, or `nonode@nohost` while it has none.
     */
    public Atom name() {
        return VirtualMachine.instance().node();
    }

    /**
     * A reading of the node's runtime counters: processes, ports, run queue
     * depth, reductions, mailbox messages, scheduler utilisation and uptime.
     * The fields and the text format are documented in docs/operations.md.
     */
    public Metrics metrics() {
        ensureOpen();
        return Metrics.snapshot();
    }

    /**
     * Call an exported Erlang function.
     */
    public Term apply(String module, String function, Term... arguments) {
        ensureOpen();
        return Bridge.call(module, function, arguments);
    }

    /**
     * Apply an Erlang fun. The call goes through erlang:apply/2, so the fun
     * runs in a process of its own instead of on the caller's thread.
     */
    public Term apply(Fun fun, Term... arguments) {
        ensureOpen();
        return Bridge.call("erlang", "apply", fun, List.of(arguments));
    }

    /**
     * Make a Java lambda callable from Erlang as module:function/arity.
     */
    public Node registerFunction(String moduleName, String function, int arity,
        Function<Term[], Term> body) {
        ensureOpen();
        if (body == null) {
            throw new IllegalArgumentException("body must not be null");
        }
        Atom name = Atom.of(moduleName);
        Module module = ModuleRegistry.loaded(name);
        if (module == null) {
            module = Module.empty(name);
            ModuleRegistry.register(module);
        }
        module.export(function, arity, body);
        return this;
    }

    /**
     * Drop the java module, drain the object handle table and stop the
     * schedulers. Closing twice does nothing the second time.
     */
    @Override
    public synchronized void close() {
        if (closed) {
            return;
        }
        closed = true;
        ModuleRegistry.unregister(Atom.of("java"));
        VirtualMachine virtualMachine = VirtualMachine.instance();
        virtualMachine.shutdown();
        virtualMachine.handleTable().setMaxHandles(HandleTable.DEFAULT_MAX_HANDLES);
    }

    private void ensureOpen() {
        if (closed) {
            throw new IllegalStateException("node is closed");
        }
    }

}
