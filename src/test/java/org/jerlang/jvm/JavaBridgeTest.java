package org.jerlang.jvm;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.jerlang.AbstractVirtualMachineTest;
import org.jerlang.FunctionSignature;
import org.jerlang.Module;
import org.jerlang.ModuleRegistry;
import org.jerlang.api.Bridge;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Atom;
import org.jerlang.type.Binary;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Str;
import org.jerlang.type.Term;
import org.junit.Test;

public class JavaBridgeTest extends AbstractJavaInteropTest {

    private static Term apply(String function, Term... args) {
        Module module = ModuleRegistry.get(Atom.of("java"));
        return module.apply(new FunctionSignature(module, function, args.length), List.of(args));
    }

    @Test
    public void testNewAndInstanceCall() {
        Term builder = apply("new", Str.of("java.lang.StringBuilder"), List.nil);
        apply("call", builder, Str.of("append"), List.of(Str.of("hello world")));
        assertEquals(Str.of("hello world"), apply("call", builder, Str.of("toString"), List.nil));
    }

    @Test
    public void testStaticCall() {
        Term result = apply("call_static", Str.of("java.lang.Math"),
            Str.of("abs"), List.of(Integer.of(-42)));
        assertEquals(Integer.of(42), result);
    }

    @Test
    public void testStaticField() {
        assertEquals(Integer.of(java.lang.Integer.MAX_VALUE),
            apply("get_static", Str.of("java.lang.Integer"), Str.of("MAX_VALUE")));
    }

    @Test
    public void testBinaryToJavaString() {
        Term string = apply("new", Str.of("java.lang.String"),
            List.of(Binary.of((byte) 'h', (byte) 'i')));
        assertEquals(Integer.of(2), apply("call", string, Str.of("length"), List.nil));
        assertEquals(Str.of("hi"), apply("call", string, Str.of("toString"), List.nil));
    }

    @Test
    public void testErlangListAsJavaCollection() {
        Term list = apply("new", Str.of("java.util.ArrayList"), List.nil);
        apply("call", list, Str.of("addAll"),
            List.of(List.of(Str.of("a"), Str.of("b"))));
        assertEquals(Integer.of(2), apply("call", list, Str.of("size"), List.nil));
    }

    @Test
    public void testJavaExceptionBecomesError() {
        try {
            apply("call_static", Str.of("java.lang.Integer"),
                Str.of("parseInt"), List.of(Str.of("nope")));
            fail("expected an Erlang error");
        } catch (Error e) {
            assertTrue(e.reason().toString(), e.reason().toString().contains("NumberFormatException"));
        }
    }

    @Test
    public void testReleaseInvalidatesHandle() {
        Term builder = apply("new", Str.of("java.lang.StringBuilder"), List.nil);
        assertEquals(Atom.of("true"), apply("release", builder));
        try {
            apply("call", builder, Str.of("toString"), List.nil);
            fail("expected an error after release");
        } catch (Error e) {
            assertTrue(e.reason().toString(), e.reason().toString().contains("not a live object handle"));
        }
    }

    @Test
    public void testErlangFromJava() {
        Term reversed = Bridge.call("lists", "reverse",
            List.of(Integer.of(1), Integer.of(2), Integer.of(3)));
        assertEquals(List.of(Integer.of(3), Integer.of(2), Integer.of(1)), reversed);
    }

    @Test
    public void testTermConversion() {
        assertEquals(Str.of("abc"), Bridge.toTerm("abc"));
        assertEquals(Integer.of(7), Bridge.toTerm(7));
        assertEquals(List.of(Integer.of(1), Integer.of(2)),
            Bridge.toTerm(java.util.Arrays.asList(1, 2)));
        assertEquals("abc", Bridge.toJava(Str.of("abc")));
    }

}
