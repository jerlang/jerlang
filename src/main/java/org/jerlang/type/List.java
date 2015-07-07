package org.jerlang.type;

public class List extends Term {

    public static final Nil nil = new Nil();

    private Term head;
    private List tail;

    public List() {
        this(nil);
    }

    public List(Term head) {
        this.head = head;
        this.tail = nil;
    }

    public List(Term head, List tail) {
        this.head = head;
        this.tail = tail;
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
                || (head.equals(other.head) && tail.equals(other.tail));
        }
        return false;
    }

    @Override
    public String toString() {
        if (head() == null) {
            return "[]";
        }
        StringBuilder stringBuilder = new StringBuilder("[");
        List list = this;
        do {
            stringBuilder.append(list.head());
            if (list.tail() == nil) {
                break;
            }
            stringBuilder.append(", ");
            list = list.tail();
        } while (true);
        stringBuilder.append(']');
        return stringBuilder.toString();
    }

}
