package org.jerlang.jvm;

import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.Map;

import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Atom;
import org.jerlang.type.Reference;

/**
 * = Object handles
 *
 * The Java objects that Erlang code reaches through a reference.
 *
 * One table belongs to each virtual machine, so a handle lives exactly as long
 * as the node that created it. The table is bounded: creating a handle past
 * the cap raises `system_limit`, the reason Erlang already uses for an
 * exhausted runtime resource, and never `{java_exception, _, _}`, which is
 * reserved for exceptions thrown by Java code.
 *
 * There is no finalizer and no `Cleaner`. `java:release/1` releases early and
 * the VM drains the whole table on shutdown.
 */
public final class HandleTable {

    /**
     * The cap used by a node that does not configure one.
     */
    public static final int DEFAULT_MAX_HANDLES = 100_000;

    /**
     * Handles handed to Erlang, mapped to the Java objects they denote.
     */
    private final Map<Reference, Object> handles = new HashMap<>();

    /**
     * Reverse lookup, so the same Java object always yields the same handle.
     */
    private final Map<Object, Reference> identities = new IdentityHashMap<>();

    private int maxHandles = DEFAULT_MAX_HANDLES;

    /**
     * Returns the handle for an object, creating one if this object has none.
     * An object that already has a live handle does not consume another slot.
     */
    public synchronized Reference register(Object object) {
        Reference existing = identities.get(object);
        if (existing != null && handles.containsKey(existing)) {
            return existing;
        }
        if (handles.size() >= maxHandles) {
            throw new Error(Atom.of("system_limit"));
        }
        Reference reference = new Reference();
        handles.put(reference, object);
        identities.put(object, reference);
        return reference;
    }

    /**
     * The object a handle denotes, or null once it was released.
     */
    public synchronized Object lookup(Reference reference) {
        return handles.get(reference);
    }

    /**
     * Early release. Returns the object that was dropped, or null.
     */
    public synchronized Object remove(Reference reference) {
        Object object = handles.remove(reference);
        if (object != null) {
            identities.remove(object);
        }
        return object;
    }

    /**
     * Drops every handle. A node starts with an empty table and drains it on
     * shutdown.
     */
    public synchronized void clear() {
        handles.clear();
        identities.clear();
    }

    public synchronized int size() {
        return handles.size();
    }

    public synchronized int maxHandles() {
        return maxHandles;
    }

    public synchronized void setMaxHandles(int max) {
        if (max < 1) {
            throw new IllegalArgumentException("maxHandles must be positive: " + max);
        }
        this.maxHandles = max;
    }

}
