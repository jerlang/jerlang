package org.jerlang;

import java.util.HashMap;
import java.util.Map;

import org.jerlang.type.Atom;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;

public class ProcessDictionary {

    private static final Atom undefined = Atom.of("undefined");
    private final Map<Term, Term> map;

    public ProcessDictionary() {
        map = new HashMap<>();
    }

    /**
     * Adds a new Key to the process dictionary, associated with the value Val,
     * and returns undefined. If Key already exists, the old value is deleted
     * and replaced by Val and the function returns the old value.
     *
     * http://www.erlang.org/doc/man/erlang.html#put-2
     */
    public Term put(Term key, Term value) {
        return value(map.put(key, value));
    }

    /**
     * Returns the value Val associated with Key in the process dictionary,
     * or undefined if Key does not exist.
     *
     * http://www.erlang.org/doc/man/erlang.html#get-1
     */
    public Term get(Term key) {
        return value(map.get(key));
    }

    /**
     * Returns the process dictionary as a list of {Key, Val} tuples.
     *
     * http://www.erlang.org/doc/man/erlang.html#get-0
     */
    public List get() {
        List list = List.nil;
        for (Map.Entry<Term, Term> entry : map.entrySet()) {
            list = new List(Tuple.of(entry.getKey(), entry.getValue()), list);
        }
        return list;
    }

    /**
     * Returns a list of keys all keys present in the process dictionary.
     *
     * http://www.erlang.org/doc/man/erlang.html#get_keys-0
     */
    public List get_keys() {
        List list = List.nil;
        for (Term key : map.keySet()) {
            list = new List(key, list);
        }
        return list;
    }

    /**
     * Returns a list of keys which are associated with the value Val in
     * the process dictionary.
     *
     * http://www.erlang.org/doc/man/erlang.html#get_keys-1
     */
    public List get_keys(Term value) {
        List list = List.nil;
        for (Map.Entry<Term, Term> entry : map.entrySet()) {
            if (entry.getValue().equals(value)) {
                list = new List(entry.getKey(), list);
            }
        }
        return list;
    }

    /**
     * Returns the process dictionary and deletes it.
     *
     * http://www.erlang.org/doc/man/erlang.html#erase-0
     */
    public Term erase() {
        List list = get();
        map.clear();
        return list;
    }

    /**
     * Returns the value Val associated with Key and deletes it from the
     * process dictionary.
     * Returns undefined if no value is associated with Key.
     *
     * http://www.erlang.org/doc/man/erlang.html#erase-1
     */
    public Term erase(Term key) {
        return value(map.remove(key));
    }

    private Term value(Term value) {
        return (value == null) ? undefined : value;
    }

}
