package org.jerlang.stdlib.ets;

import org.jerlang.type.Atom;
import org.jerlang.type.Term;

import com.googlecode.totallylazy.collections.PersistentSet;
import com.googlecode.totallylazy.collections.PersistentSortedSet;

public class Set extends Table {

    private PersistentSet<Term> set;

    public Set(Access access, Atom name, boolean named_table, boolean compressed) {
        super(Type.SET, access, name, named_table, compressed);
        set = PersistentSortedSet.constructors.sortedSet();
    }

}
