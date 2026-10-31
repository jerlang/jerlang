package org.jerlang.jvm;

import java.lang.reflect.Array;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;

import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Atom;
import org.jerlang.type.Binary;
import org.jerlang.type.Float;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Reference;
import org.jerlang.type.Str;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;
import org.jerlang.vm.VirtualMachine;

/**
 * = Java bridge
 *
 * Calls Java code from Erlang terms and converts values in both directions.
 *
 * Because the Erlang runtime runs on the JVM itself, a call is a plain
 * reflective invocation: no external term format, no sockets, no separate
 * process. References handed out by this class keep the Java object alive
 * until it is released with java:release/1 or the node shuts down; the table
 * itself belongs to the virtual machine.
 */
public final class JavaBridge {

    private static final Object NO_MATCH = new Object();

    private JavaBridge() {
    }

    /**
     * The handle table of the running node. There is always a VM instance, so
     * a bridge call outside a started node still has a table; it is simply
     * empty and is drained again when the node shuts down.
     */
    private static HandleTable handles() {
        return VirtualMachine.instance().handleTable();
    }

    // ----------------------------------------------------------------
    // Erlang API
    // ----------------------------------------------------------------

    public static Term newObject(Term className, Term arguments) {
        Class<?> clazz = resolveClass(className);
        List args = arguments.toList();
        Constructor<?> best = null;
        Object[] bestArgs = null;
        int bestScore = java.lang.Integer.MAX_VALUE;
        for (Constructor<?> constructor : clazz.getConstructors()) {
            if (constructor.getParameterCount() != args.length()) {
                continue;
            }
            int[] score = new int[1];
            Object[] converted = convert(args, constructor.getParameterTypes(), score);
            if (converted != null && score[0] < bestScore) {
                best = constructor;
                bestArgs = converted;
                bestScore = score[0];
            }
        }
        if (best == null) {
            throw new Error(Str.of("java: no matching constructor for "
                + clazz.getName() + "/" + args.length()));
        }
        try {
            return register(best.newInstance(bestArgs));
        } catch (InvocationTargetException e) {
            throw javaError(e.getCause() == null ? e : e.getCause());
        } catch (ReflectiveOperationException | IllegalArgumentException e) {
            throw javaError(e);
        }
    }

    public static Term call(Term handle, Term methodName, Term arguments) {
        Object target = resolve(handle);
        return invoke(target.getClass(), target, requireString(methodName, "method"), arguments);
    }

    public static Term callStatic(Term className, Term methodName, Term arguments) {
        return invoke(resolveClass(className), null, requireString(methodName, "method"), arguments);
    }

    public static Term get(Term handle, Term fieldName) {
        Object target = resolve(handle);
        return readField(target.getClass(), target, requireString(fieldName, "field"));
    }

    public static Term getStatic(Term className, Term fieldName) {
        return readField(resolveClass(className), null, requireString(fieldName, "field"));
    }

    public static Term set(Term handle, Term fieldName, Term value) {
        Object target = resolve(handle);
        writeField(target.getClass(), target, requireString(fieldName, "field"), value);
        return Atom.of("true");
    }

    public static Term setStatic(Term className, Term fieldName, Term value) {
        writeField(resolveClass(className), null, requireString(fieldName, "field"), value);
        return Atom.of("true");
    }

    public static Term release(Term handle) {
        if (handle instanceof Reference) {
            handles().remove((Reference) handle);
        }
        return Atom.of("true");
    }

    // ----------------------------------------------------------------
    // Reflection
    // ----------------------------------------------------------------

    private static Term invoke(Class<?> clazz, Object target, String name, Term arguments) {
        List args = arguments.toList();
        Method best = null;
        Object[] bestArgs = null;
        int bestScore = java.lang.Integer.MAX_VALUE;
        for (Method method : clazz.getMethods()) {
            if (!method.getName().equals(name) || method.getParameterCount() != args.length()) {
                continue;
            }
            if (target == null && !Modifier.isStatic(method.getModifiers())) {
                continue;
            }
            if (target != null && Modifier.isStatic(method.getModifiers())) {
                continue;
            }
            int[] score = new int[1];
            Object[] converted = convert(args, method.getParameterTypes(), score);
            if (converted != null && score[0] < bestScore) {
                best = method;
                bestArgs = converted;
                bestScore = score[0];
            }
        }
        if (best == null) {
            throw new Error(Str.of("java: no matching method "
                + clazz.getName() + ":" + name + "/" + args.length()));
        }
        try {
            return toTerm(best.invoke(target, bestArgs));
        } catch (InvocationTargetException e) {
            throw javaError(e.getCause() == null ? e : e.getCause());
        } catch (ReflectiveOperationException | IllegalArgumentException e) {
            throw javaError(e);
        }
    }

    private static Term readField(Class<?> clazz, Object target, String name) {
        try {
            Field field = clazz.getField(name);
            return toTerm(field.get(target));
        } catch (ReflectiveOperationException e) {
            throw javaError(e);
        }
    }

    private static void writeField(Class<?> clazz, Object target, String name, Term value) {
        try {
            Field field = clazz.getField(name);
            Object converted = coerce(value, field.getType());
            if (converted == NO_MATCH) {
                throw new Error(Str.of("java: cannot assign " + value
                    + " to " + field.getType().getName()));
            }
            field.set(target, converted);
        } catch (ReflectiveOperationException e) {
            throw javaError(e);
        }
    }

    private static Object[] convert(List args, Class<?>[] types, int[] score) {
        Object[] converted = new Object[types.length];
        List rest = args;
        int total = 0;
        for (int index = 0; index < types.length; index++) {
            Term term = rest.head();
            Object value = coerce(term, types[index]);
            if (value == NO_MATCH) {
                return null;
            }
            converted[index] = value;
            total += distance(term, value, types[index]);
            rest = rest.tail();
        }
        score[0] = total;
        return converted;
    }

    /**
     * Lower is better. Preference order for an Erlang integer is
     * int, then the other integral types, then floating point.
     */
    private static int distance(Term term, Object value, Class<?> target) {
        if (target == Object.class) {
            return 4;
        }
        if (target == boolean.class || target == Boolean.class) {
            return 0;
        }
        if (target.isPrimitive()) {
            if (term instanceof Integer) {
                if (target == int.class) {
                    return 0;
                }
                if (target == long.class || target == short.class || target == byte.class
                    || target == char.class) {
                    return 1;
                }
                return 2;
            }
            if (term instanceof Float) {
                if (target == double.class || target == float.class) {
                    return 0;
                }
                return 3;
            }
            return 1;
        }
        if (value != null && target.equals(value.getClass())) {
            return 0;
        }
        if (target.isInstance(value)) {
            return 1;
        }
        return 2;
    }

    // ----------------------------------------------------------------
    // Conversion: Erlang term -> Java value
    // ----------------------------------------------------------------

    /**
     * Converts a term to a value that can be passed to a parameter of the
     * given type, or returns NO_MATCH if the term does not fit.
     */
    private static Object coerce(Term term, Class<?> target) {
        if (target == Term.class) {
            return term;
        }
        if (target == Object.class) {
            return toJava(term);
        }
        if (term instanceof Reference) {
            Object object = handles().lookup((Reference) term);
            return object != null && target.isInstance(object) ? object : NO_MATCH;
        }
        if (target == String.class || target == CharSequence.class) {
            String string = asString(term);
            return string == null ? NO_MATCH : string;
        }
        if (target == boolean.class || target == Boolean.class) {
            if (term instanceof Atom) {
                if (term.toString().equals("true")) {
                    return java.lang.Boolean.TRUE;
                }
                if (term.toString().equals("false")) {
                    return java.lang.Boolean.FALSE;
                }
            }
            return NO_MATCH;
        }
        if (target == char.class || target == Character.class) {
            if (term instanceof Integer) {
                int value = ((Integer) term).toInt();
                if (value >= Character.MIN_VALUE && value <= Character.MAX_VALUE) {
                    return (char) value;
                }
            }
            String string = asString(term);
            if (string != null && string.length() == 1) {
                return string.charAt(0);
            }
            return NO_MATCH;
        }
        if (target.isEnum() && term instanceof Atom) {
            return enumValue(target, term.toString());
        }
        if (term instanceof Integer) {
            Object number = coerceNumber((Integer) term, target);
            if (number != NO_MATCH) {
                return number;
            }
        }
        if (term instanceof Float) {
            Object number = coerceNumber((Float) term, target);
            if (number != NO_MATCH) {
                return number;
            }
        }
        if (target == byte[].class) {
            if (term instanceof Binary) {
                return ((Binary) term).toByteArray();
            }
            if (term instanceof Str) {
                return ((Str) term).toByteArray();
            }
            return NO_MATCH;
        }
        if (target.isArray()) {
            return coerceArray(term, target.getComponentType());
        }
        if (Collection.class.isAssignableFrom(target) || target == Iterable.class) {
            if (term instanceof List && !(term instanceof Str)) {
                return toJavaList((List) term);
            }
            return NO_MATCH;
        }
        Object value = toJava(term);
        return value != null && target.isInstance(value) ? value : NO_MATCH;
    }

    @SuppressWarnings({ "unchecked", "rawtypes" })
    private static Object enumValue(Class<?> target, String name) {
        try {
            return Enum.valueOf((Class<? extends Enum>) target, name);
        } catch (IllegalArgumentException e) {
            return NO_MATCH;
        }
    }

    private static Object coerceNumber(Integer term, Class<?> target) {
        BigInteger value = term.toBigInteger();
        if (target == int.class || target == java.lang.Integer.class) {
            return value.bitLength() < 32 ? (Object) value.intValue() : NO_MATCH;
        }
        if (target == long.class || target == java.lang.Long.class) {
            return value.bitLength() < 64 ? (Object) value.longValue() : NO_MATCH;
        }
        if (target == short.class || target == java.lang.Short.class) {
            return value.bitLength() < 16 ? (Object) value.shortValue() : NO_MATCH;
        }
        if (target == byte.class || target == java.lang.Byte.class) {
            return value.bitLength() < 8 ? (Object) value.byteValue() : NO_MATCH;
        }
        if (target == double.class || target == java.lang.Double.class) {
            return value.doubleValue();
        }
        if (target == float.class || target == java.lang.Float.class) {
            return value.floatValue();
        }
        if (target == BigInteger.class) {
            return value;
        }
        if (target == BigDecimal.class) {
            return new BigDecimal(value);
        }
        if (target == Number.class) {
            return value.bitLength() < 32 ? (Object) value.intValue() : (Object) value.longValue();
        }
        return NO_MATCH;
    }

    private static Object coerceNumber(Float term, Class<?> target) {
        double value = term.toBigDecimal().doubleValue();
        if (target == double.class || target == java.lang.Double.class) {
            return value;
        }
        if (target == float.class || target == java.lang.Float.class) {
            return (float) value;
        }
        if (target == BigDecimal.class) {
            return term.toBigDecimal();
        }
        if (target == Number.class) {
            return value;
        }
        return NO_MATCH;
    }

    private static Object coerceArray(Term term, Class<?> component) {
        int length;
        if (term instanceof Tuple) {
            length = ((Tuple) term).arity();
        } else if (term instanceof List && !(term instanceof Str)) {
            length = ((List) term).length();
        } else {
            return NO_MATCH;
        }
        Object array = Array.newInstance(component, length);
        List rest = term instanceof List ? (List) term : null;
        for (int index = 0; index < length; index++) {
            Term element = rest != null ? rest.head() : ((Tuple) term).element(index + 1);
            Object value = coerce(element, component);
            if (value == NO_MATCH) {
                return NO_MATCH;
            }
            Array.set(array, index, value);
            if (rest != null) {
                rest = rest.tail();
            }
        }
        return array;
    }

    /**
     * Generic conversion used when no target type constrains the result.
     */
    public static Object toJava(Term term) {
        if (term instanceof Reference) {
            Object object = handles().lookup((Reference) term);
            return object == null ? term : object;
        }
        if (term instanceof Str) {
            return ((Str) term).string();
        }
        if (term instanceof Atom) {
            if (term.toString().equals("true")) {
                return java.lang.Boolean.TRUE;
            }
            if (term.toString().equals("false")) {
                return java.lang.Boolean.FALSE;
            }
            if (term.toString().equals("undefined")) {
                return null;
            }
            return term.toString();
        }
        if (term instanceof Integer) {
            BigInteger value = ((Integer) term).toBigInteger();
            if (value.bitLength() < 32) {
                return value.intValue();
            }
            if (value.bitLength() < 64) {
                return value.longValue();
            }
            return value;
        }
        if (term instanceof Float) {
            return ((Float) term).toBigDecimal().doubleValue();
        }
        if (term instanceof Binary) {
            return ((Binary) term).toByteArray();
        }
        if (term instanceof Tuple) {
            Tuple tuple = (Tuple) term;
            Object[] array = new Object[tuple.arity()];
            for (int index = 0; index < array.length; index++) {
                array[index] = toJava(tuple.element(index + 1));
            }
            return array;
        }
        if (term instanceof org.jerlang.type.Map) {
            org.jerlang.type.Map map = (org.jerlang.type.Map) term;
            Map<Object, Object> result = new LinkedHashMap<>();
            for (List keys = map.keys(); keys.length() > 0; keys = keys.tail()) {
                Term key = keys.head();
                result.put(toJava(key), toJava(map.get(key)));
            }
            return result;
        }
        if (term instanceof List) {
            return toJavaList((List) term);
        }
        return term;
    }

    private static ArrayList<Object> toJavaList(List list) {
        ArrayList<Object> result = new ArrayList<>(list.length());
        for (List rest = list; rest.length() > 0; rest = rest.tail()) {
            result.add(toJava(rest.head()));
        }
        return result;
    }

    // ----------------------------------------------------------------
    // Conversion: Java value -> Erlang term
    // ----------------------------------------------------------------

    public static Term toTerm(Object object) {
        if (object == null) {
            return Atom.of("undefined");
        }
        if (object instanceof Term) {
            return (Term) object;
        }
        if (object instanceof java.lang.Boolean) {
            return Atom.of(object.toString());
        }
        if (object instanceof String) {
            return Str.of((String) object);
        }
        if (object instanceof Character) {
            return Str.of(object.toString());
        }
        if (object instanceof Byte || object instanceof Short || object instanceof java.lang.Integer
            || object instanceof java.lang.Long) {
            return integer(((Number) object).longValue());
        }
        if (object instanceof BigInteger) {
            return new Integer((BigInteger) object);
        }
        if (object instanceof java.lang.Float || object instanceof java.lang.Double) {
            return Float.of(((Number) object).doubleValue());
        }
        if (object instanceof byte[]) {
            return Binary.of((byte[]) object);
        }
        if (object instanceof Object[]) {
            Object[] array = (Object[]) object;
            Term[] terms = new Term[array.length];
            for (int index = 0; index < array.length; index++) {
                terms[index] = toTerm(array[index]);
            }
            return Tuple.of(terms);
        }
        if (object instanceof java.util.Map) {
            org.jerlang.type.Map map = new org.jerlang.type.Map();
            for (Map.Entry<?, ?> entry : ((java.util.Map<?, ?>) object).entrySet()) {
                map.set(toTerm(entry.getKey()), toTerm(entry.getValue()));
            }
            return map;
        }
        if (object instanceof Collection) {
            Collection<?> collection = (Collection<?>) object;
            Term[] terms = new Term[collection.size()];
            int index = 0;
            for (Object element : collection) {
                terms[index++] = toTerm(element);
            }
            return List.of(terms);
        }
        return register(object);
    }

    private static Integer integer(long value) {
        if (value >= java.lang.Integer.MIN_VALUE && value <= java.lang.Integer.MAX_VALUE) {
            return Integer.of((int) value);
        }
        return new Integer(BigInteger.valueOf(value));
    }

    // ----------------------------------------------------------------
    // Handles and errors
    // ----------------------------------------------------------------

    private static Object resolve(Term handle) {
        Object object = handle instanceof Reference ? handles().lookup((Reference) handle) : null;
        if (object == null) {
            throw new Error(Str.of("java: not a live object handle: " + handle));
        }
        return object;
    }

    private static Reference register(Object object) {
        return handles().register(object);
    }

    private static Class<?> resolveClass(Term name) {
        String className = asString(name);
        if (className == null) {
            throw new Error(Str.of("java: invalid class name: " + name));
        }
        switch (className) {
        case "boolean":
            return boolean.class;
        case "byte":
            return byte.class;
        case "char":
            return char.class;
        case "double":
            return double.class;
        case "float":
            return float.class;
        case "int":
            return int.class;
        case "long":
            return long.class;
        case "short":
            return short.class;
        case "void":
            return void.class;
        default:
            break;
        }
        try {
            return Class.forName(className);
        } catch (ClassNotFoundException e) {
            throw new Error(Str.of("java: unknown class: " + className));
        }
    }

    private static String requireString(Term term, String what) {
        String string = asString(term);
        if (string == null) {
            throw new Error(Str.of("java: invalid " + what + " name: " + term));
        }
        return string;
    }

    private static String asString(Term term) {
        if (term instanceof Str) {
            return ((Str) term).string();
        }
        if (term instanceof Atom) {
            return term.toString();
        }
        if (term instanceof List) {
            return Str.convert((List) term).string();
        }
        return null;
    }

    /**
     * Java exceptions become Erlang errors with reason
     * {java_exception, ClassName, Message}.
     */
    private static Error javaError(Throwable t) {
        String message = t.getMessage() == null ? "" : t.getMessage();
        return new Error(Tuple.of(
            Atom.of("java_exception"),
            Str.of(t.getClass().getName()),
            Str.of(message)));
    }

}
