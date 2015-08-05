package org.jerlang.type;

import java.util.Collection;
import java.util.Objects;

public class List extends Term {

    public static final Nil nil = new Nil();

    private final Term head;
    private final List tail;
    private final int length;

    public List() {
        this(nil);
    }

    public List(Term head) {
        this(head, nil);
    }

    public List(Term head, List tail) {
        this.head = head;
        this.tail = tail;
        if (tail == null) {
            this.length = 1;
        } else {
            this.length = 1 + tail.length();
        }
    }

    public Term head() {
        return head;
    }

    public List tail() {
        return tail;
    }

    @Override
    public boolean equals(Object object) {
        if (object instanceof List) {
            List other = (List) object;
            return (head == null && other.head == null)
                || (head != null && head.equals(other.head) && tail.equals(other.tail));
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Objects.hash(head, tail);
    }

    public int length() {
        return length;
    }

    @Override
    public List toList() {
        return this;
    }

    @Override
    public String toString() {
        if (head() == null) {
            return "[]";
        }
        StringBuilder stringBuilder = new StringBuilder("[");
        List list = this;
        while (list.length() > 0) {
            stringBuilder.append(list.head());
            if (list.tail() == nil) {
                break;
            }
            stringBuilder.append(",");
            list = list.tail();
        }
        stringBuilder.append(']');
        return stringBuilder.toString();
    }

    /**
     * Convenience function to create a list of terms.
     */
    public static List of(Term... terms) {
        if (terms.length == 0) {
            return nil;
        }
        List list = nil;
        for (int index = terms.length - 1; index >= 0; index--) {
            list = new List(terms[index], list);
        }
        return list;
    }

    public static List of(Collection<? extends Term> collection) {
        List list = nil;
        for (Term term : collection) {
            list = new List(term, list);
        }
        return list;
    }

}
