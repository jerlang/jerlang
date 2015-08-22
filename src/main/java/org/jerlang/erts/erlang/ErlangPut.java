package org.jerlang.erts.erlang;

import org.jerlang.Process;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.jerlang.vm.VirtualMachine;

/**
 * Adds a new Key to the process dictionary,
 * associated with the value Val,
 * and returns undefined.
 * If Key already exists,
 * the old value is deleted and replaced by Val and
 * the function returns the old value.
 *
 * http://www.erlang.org/doc/man/erlang.html#put-2
 */
public class ErlangPut {

    private ErlangPut() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 2:
            Term key = params.head();
            params = params.tail();
            Term val = params.head();
            return put_2(key, val);
        default:
            throw new Error("badarg");
        }
    }

    public static Term put_2(Term key, Term val) {
        Process self = VirtualMachine.instance().self();
        return self.dictionary().put(key, val);
    }

}
