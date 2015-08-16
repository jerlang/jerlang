package org.jerlang.type;

import java.util.Objects;
import java.util.concurrent.atomic.AtomicLong;

/**
 * References are Erlang objects with exactly two properties:
 *
 * * They can be created by a program (using make_ref/0), and,
 * * They can be compared for equality.
 *
 * Erlang references are unique, the system guarantees that no two
 * references created by different calls to make_ref will ever match.
 *
 * Source: http://www.erlang.org/course/advanced.html#refs
 */
public class Reference extends Term {

    private static final AtomicLong counter = new AtomicLong();

    private final long id;

    public Reference() {
        this.id = counter.incrementAndGet();
    }

    @Override
    public boolean equals(Object object) {
        if (object instanceof Reference) {
            Reference other = (Reference) object;
            return id == other.id;
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Objects.hash(id);
    }

    @Override
    public Reference toReference() {
        return this;
    }

    @Override
    public String toString() {
        return "#Ref<0." + id + ".0>";
    }

}
