package org.jerlang.stdlib.ets;

import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Atom;
import org.jerlang.type.List;
import org.jerlang.type.Term;

public class EtsNew {

    private static final Atom compressed = Atom.of("compressed");
    private static final Atom named_table = Atom.of("named_table");

    private EtsNew() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 2:
            Atom name = params.head().toAtom();
            params = params.tail();
            List options = params.head().toList();
            return new_2(name, options);
        default:
            throw Error.badarg;
        }
    }

    /**
     * Creates a new table and returns a table identifier which can be used
     * in subsequent operations. The table identifier can be sent to other
     * processes so that a table can be shared between different processes
     * within a node.
     *
     * The parameter Options is a list of atoms which specifies table type,
     * access rights, key position and if the table is named or not.
     * If one or more options are left out, the default values are used.
     * This means that not specifying any options ([]) is the same as
     * specifying
     * [set, protected, {keypos,1}, {heir,none}, {write_concurrency,false},
     * {read_concurrency,false}].
     *
     * http://www.erlang.org/doc/man/ets.html#new-2
     */
    public static Term new_2(Atom name, List options) {
        Type type = Type.SET;
        Access access = Access.PROTECTED;
        boolean is_compressed = false;
        boolean is_named_table = false;

        // Parse options ------------------------------------------------------

        while (options.length() > 0) {
            Term option = options.head();

            if (Type.SET.toAtom().equals(option)) {
                type = Type.SET;
            } else if (Type.ORDERED_SET.toAtom().equals(option)) {
                type = Type.ORDERED_SET;
            } else if (Type.BAG.toAtom().equals(option)) {
                type = Type.BAG;
            } else if (Type.DUPLICATE_BAG.toAtom().equals(option)) {
                type = Type.DUPLICATE_BAG;
            } else if (Access.PUBLIC.toAtom().equals(option)) {
                access = Access.PUBLIC;
            } else if (Access.PROTECTED.toAtom().equals(option)) {
                access = Access.PROTECTED;
            } else if (Access.PRIVATE.toAtom().equals(option)) {
                access = Access.PRIVATE;
            } else if (named_table.equals(option)) {
                is_named_table = true;
            } else if (compressed.equals(option)) {
                is_compressed = true;
            }
            options = options.tail();
        }

        switch (type) {
        case SET:
            return new Set(access, name, is_named_table, is_compressed).id();
        default:
            throw new Error("not implemented yet");
        }
    }

}
