package org.jerlang.stdlib.ets;

import java.util.concurrent.atomic.AtomicInteger;

import org.jerlang.type.Atom;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Base class for all ETS tables.
 */
public abstract class Table {

    private static final AtomicInteger COUNTER = new AtomicInteger();

    private final Integer id;
    private final Type type;
    private final Access access;
    private final Atom name;
    private final boolean named_table;
    private final boolean compressed;

    protected Table(Type type, Access access, Atom name, boolean named_table, boolean compressed) {
        this.id = Integer.of(COUNTER.incrementAndGet());
        this.type = type;
        this.access = access;
        this.name = name;
        this.named_table = named_table;
        this.compressed = compressed;
        TableRegistry.register(this);
    }

    public Access access() {
        return access;
    }

    public boolean compressed() {
        return compressed;
    }

    public Integer id() {
        return id;
    }

    public abstract List lookup(Term key);

    public Atom name() {
        return name;
    }

    public boolean named_table() {
        return named_table;
    }

    public Type type() {
        return type;
    }

}
