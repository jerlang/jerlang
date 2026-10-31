package org.jerlang.jvm;

import static org.junit.Assert.assertEquals;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;

import org.jerlang.Module;
import org.jerlang.ModuleRegistry;
import org.jerlang.erts.Erlang;
import org.jerlang.stdlib.Java;
import org.jerlang.type.Atom;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Str;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;
import org.jerlang.vm.VirtualMachine;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * Runs bridge_demo.beam, which was compiled from src/test/erl/bridge_demo.erl
 * by Erlang/OTP 29, and lets that module call the Java bridge.
 */
public class ErlangSourceBridgeTest {

    private static final Path SOURCE = Paths.get("src/test/resources/bridge_demo.beam");
    private static final Path LOCAL = Paths.get("bridge_demo.beam");

    @Before
    public void start() throws Exception {
        Files.copy(SOURCE, LOCAL, StandardCopyOption.REPLACE_EXISTING);
        VirtualMachine.instance().start();
        // The java module is opt-in now, so this node registers it as a node
        // with reflection enabled would.
        Module javaModule = new Module(Java.class, Atom.of("java"));
        javaModule.export();
        ModuleRegistry.register(javaModule);
    }

    @After
    public void stop() throws Exception {
        VirtualMachine.instance().shutdown();
        Files.deleteIfExists(LOCAL);
    }

    @Test
    public void testGreetingFromCompiledErlang() throws Throwable {
        Term result = Erlang.apply(Atom.of("bridge_demo"), Atom.of("greeting"),
            List.of(Str.of("erlang")));
        assertEquals(Str.of("hello, erlang"), result);
    }

    @Test
    public void testStaticCallAndTupleFromCompiledErlang() throws Throwable {
        Term result = Erlang.apply(Atom.of("bridge_demo"), Atom.of("run"), List.nil);
        assertEquals(Tuple.of(Str.of("hello, erlang"), Integer.of(7)), result);
    }

}
