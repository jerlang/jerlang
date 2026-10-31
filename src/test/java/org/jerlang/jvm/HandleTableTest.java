package org.jerlang.jvm;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.jerlang.AbstractVirtualMachineTest;
import org.jerlang.FunctionSignature;
import org.jerlang.Module;
import org.jerlang.ModuleRegistry;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Atom;
import org.jerlang.type.List;
import org.jerlang.type.Str;
import org.jerlang.type.Term;
import org.jerlang.vm.VirtualMachine;
import org.junit.Test;

/**
 * The handle table belongs to the node: it is bounded, drained on shutdown
 * and starts empty, which a static table could not guarantee.
 */
public class HandleTableTest extends AbstractJavaInteropTest {

    private static Term apply(String function, Term... args) {
        Module module = ModuleRegistry.get(Atom.of("java"));
        return module.apply(new FunctionSignature(module, function, args.length), List.of(args));
    }

    private static Term newBuilder() {
        return apply("new", Str.of("java.lang.StringBuilder"), List.nil);
    }

    private static Error expectError(Runnable action) {
        try {
            action.run();
        } catch (Error e) {
            return e;
        }
        fail("expected an Erlang error");
        return null;
    }

    private static HandleTable table() {
        return VirtualMachine.instance().handleTable();
    }

    @Test
    public void tableIsBoundedAndDefaultsToOneHundredThousand() {
        assertEquals(100_000, table().maxHandles());
        try {
            table().setMaxHandles(2);
            Term first = newBuilder();
            newBuilder();
            assertEquals(2, table().size());

            Error error = expectError(HandleTableTest::newBuilder);
            assertEquals(Atom.of("system_limit"), error.reason());
            assertFalse(error.reason().toString().contains("java_exception"));
            assertEquals(2, table().size());

            // The cap counts live handles, so a release makes room again.
            assertEquals(Atom.of("true"), apply("release", first));
            assertEquals(1, table().size());
            newBuilder();
            assertEquals(2, table().size());
        } finally {
            table().setMaxHandles(HandleTable.DEFAULT_MAX_HANDLES);
        }
    }

    @Test
    public void aReleasedHandleIsNotLive() {
        Term builder = newBuilder();
        assertEquals(1, table().size());
        assertEquals(Atom.of("true"), apply("release", builder));
        assertEquals(0, table().size());

        Error error = expectError(() -> apply("call", builder, Str.of("toString"), List.nil));
        assertTrue(error.reason().toString(),
            error.reason().toString().contains("not a live object handle"));
    }

    @Test
    public void shutdownDrainsTheTableAndARestartedNodeStartsEmpty() {
        Term builder = newBuilder();
        assertEquals(1, table().size());

        // Shutting the node down drains the table. Nothing can be dispatched
        // here: without a running scheduler a call would never return.
        VirtualMachine.instance().shutdown();
        assertEquals(0, table().size());

        // A restarted node starts empty, so the handle the previous node
        // handed out no longer names anything.
        VirtualMachine.instance().start();
        assertEquals(0, table().size());
        Error error = expectError(() -> apply("call", builder, Str.of("toString"), List.nil));
        assertTrue(error.reason().toString(),
            error.reason().toString().contains("not a live object handle"));

        newBuilder();
        assertEquals(1, table().size());
    }

}
