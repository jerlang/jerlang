package org.jerlang.jvm;

import static org.junit.Assert.assertEquals;

import org.jerlang.AbstractVirtualMachineTest;
import org.jerlang.FunctionSignature;
import org.jerlang.Module;
import org.jerlang.ModuleRegistry;
import org.jerlang.type.Atom;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Str;
import org.jerlang.type.Term;
import org.junit.Test;

/**
 * A call has to pick the same overload on every JVM: conversion distance first,
 * then parameter specificity, then the sorted signature. The declarations
 * below are in an order that Class#getMethods is free to change, so nothing
 * here may depend on it.
 */
public class OverloadResolutionTest extends AbstractVirtualMachineTest {

    public static final class Overloads {

        public static String pick(Object value) {
            return "Object";
        }

        public static String pick(CharSequence value) {
            return "CharSequence";
        }

        public static String pick(String value) {
            return "String";
        }

        public static String specificity(java.util.Collection<?> value) {
            return "Collection";
        }

        public static String specificity(java.util.List<?> value) {
            return "List";
        }

        public static String signature(CharSequence value) {
            return "CharSequence";
        }

        public static String signature(Comparable<?> value) {
            return "Comparable";
        }

        public static String varargs(String prefix, Object... rest) {
            return prefix + ":" + rest.length;
        }
    }

    private static final Str OVERLOADS = Str.of("org.jerlang.jvm.OverloadResolutionTest$Overloads");

    private static Term java(String function, Term... args) {
        Module module = ModuleRegistry.get(Atom.of("java"));
        return module.apply(new FunctionSignature(module, function, args.length), List.of(args));
    }

    private static Term callStatic(String name, Term... args) {
        return java("call_static", OVERLOADS, Str.of(name), List.of(args));
    }

    @Test
    public void distancePicksTheNarrowestFit() {
        assertEquals(Str.of("String"), callStatic("pick", Str.of("hello")));
    }

    @Test
    public void aDistanceTieIsBrokenByParameterSpecificity() {
        Term list = java("new", Str.of("java.util.ArrayList"), List.nil);
        assertEquals(Str.of("List"), callStatic("specificity", list));
    }

    @Test
    public void aSpecificityTieIsBrokenBySortedSignature() {
        assertEquals(Str.of("CharSequence"), callStatic("signature", Str.of("hello")));
    }

    @Test
    public void aTrailingObjectVarargsParameterTakesTheRest() {
        assertEquals(Str.of("a:2"),
            callStatic("varargs", Str.of("a"), Integer.of(1), Integer.of(2)));
    }

}
