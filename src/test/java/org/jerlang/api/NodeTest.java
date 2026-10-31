package org.jerlang.api;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.jerlang.ModuleRegistry;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Atom;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Str;
import org.jerlang.type.Term;
import org.jerlang.vm.VirtualMachine;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * The embedding contract from docs/java-interop.md: a node owns the runtime,
 * reflection is off unless asked for, and closing it undoes both.
 */
public class NodeTest {

    @Before
    public void forgetJavaModule() {
        ModuleRegistry.unregister(Atom.of("java"));
    }

    @After
    public void resetModules() {
        ModuleRegistry.reset();
        VirtualMachine.instance().handleTable().setMaxHandles(
            org.jerlang.jvm.HandleTable.DEFAULT_MAX_HANDLES);
    }

    @Test
    public void aPlainNodeHasNoJavaModule() {
        try (Node node = Node.start()) {
            assertFalse(node.hasReflection());
            assertNull(ModuleRegistry.get(Atom.of("java")));
        }
    }

    @Test
    public void reflectionRegistersTheJavaModule() {
        try (Node node = Node.start(NodeOptions.builder().reflection(true).build())) {
            assertTrue(node.hasReflection());
            Term builder = node.apply("java", "new", Str.of("java.lang.StringBuilder"), List.nil);
            node.apply("java", "call", builder, Str.of("append"), List.of(Str.of("hi")));
            assertEquals(Str.of("hi"),
                node.apply("java", "call", builder, Str.of("toString"), List.nil));
        }
    }

    @Test
    public void aRegisteredFunctionIsCallableFromErlang() {
        try (Node node = Node.start()) {
            node.registerFunction("my_mod", "echo", 1, args -> args[0]);
            assertEquals(Integer.of(7), node.apply("my_mod", "echo", Integer.of(7)));
        }
    }

    @Test
    public void anErlangFunCanBeApplied() {
        try (Node node = Node.start()) {
            Term fun = node.apply("erlang", "make_fun",
                Atom.of("lists"), Atom.of("reverse"), Integer.of(1));
            assertEquals(List.of(Integer.of(2), Integer.of(1)),
                node.apply(fun.toFun(), Integer.of(1), Integer.of(2)));
        }
    }

    @Test
    public void maxHandlesBoundsTheTable() {
        try (Node node = Node.start(NodeOptions.builder().reflection(true).maxHandles(1).build())) {
            node.apply("java", "new", Str.of("java.lang.StringBuilder"), List.nil);
            try {
                node.apply("java", "new", Str.of("java.lang.StringBuilder"), List.nil);
                fail("a second handle should exceed the cap");
            } catch (Error e) {
                assertEquals(Atom.of("system_limit"), e.reason());
            }
        }
    }

    @Test
    public void closingReleasesHandlesAndTheJavaModule() {
        Node node = Node.start(NodeOptions.builder().reflection(true).build());
        node.apply("java", "new", Str.of("java.lang.StringBuilder"), List.nil);
        assertEquals(1, VirtualMachine.instance().handleTable().size());

        node.close();

        assertTrue(node.isClosed());
        assertEquals(0, VirtualMachine.instance().handleTable().size());
        assertNull(ModuleRegistry.get(Atom.of("java")));
        node.close();
        try {
            node.apply("lists", "reverse", List.nil);
            fail("a closed node must not run Erlang code");
        } catch (IllegalStateException e) {
            assertEquals("node is closed", e.getMessage());
        }
    }

}
