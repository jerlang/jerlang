package org.jerlang.stdlib.ets;

import java.util.Collection;
import java.util.concurrent.ConcurrentHashMap;

import org.jerlang.type.Atom;
import org.jerlang.type.Term;

public class TableRegistry {

    private static final TableRegistry INSTANCE = new TableRegistry();

    private ConcurrentHashMap<Term, Table> id2table;
    private ConcurrentHashMap<Atom, Table> name2table;

    private TableRegistry() {
        id2table = new ConcurrentHashMap<>();
        name2table = new ConcurrentHashMap<>();
    }

    public static void register(Table table) {
        INSTANCE.id2table.put(table.id(), table);
        if (table.named_table()) {
            INSTANCE.name2table.put(table.name(), table);
        }
    }

    public static Collection<Table> tables() {
        return INSTANCE.id2table.values();
    }

    public static void unregister(Table table) {
        INSTANCE.id2table.remove(table.id());
        if (table.named_table()) {
            INSTANCE.name2table.remove(table.name());
        }
    }

}
