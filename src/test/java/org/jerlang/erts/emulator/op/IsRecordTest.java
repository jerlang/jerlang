package org.jerlang.erts.emulator.op;

import static org.junit.Assert.assertEquals;

import org.jerlang.erts.Erlang;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.Atom;
import org.jerlang.type.Boolean;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;
import org.junit.Test;

public class IsRecordTest extends AbstractOpTest {

    public IsRecordTest() {
        super("is_record.beam");
    }

    @Test
    public void test() throws ThrowException {
        List params1 = List.of(List.nil, List.nil);
        Term result1 = Erlang.apply(Atom.of("is_record"), Atom.of("test"), params1);
        assertEquals(Boolean.am_false, result1);

        List params2 = List.of(Boolean.am_false, Atom.of("tag"));
        Term result2 = Erlang.apply(Atom.of("is_record"), Atom.of("test"), params2);
        assertEquals(Boolean.am_false, result2);

        Atom tag = Atom.of("tag");
        Tuple tuple = Tuple.of(tag, tag);
        List params3 = List.of(tuple, tag);
        Term result3 = Erlang.apply(Atom.of("is_record"), Atom.of("test"), params3);
        assertEquals(Boolean.am_true, result3);
    }

}
