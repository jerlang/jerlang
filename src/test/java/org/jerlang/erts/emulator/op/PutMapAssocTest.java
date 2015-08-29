package org.jerlang.erts.emulator.op;

import static org.junit.Assert.assertEquals;

import org.jerlang.erts.Erlang;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.Atom;
import org.jerlang.type.List;
import org.jerlang.type.Map;
import org.jerlang.type.Term;
import org.junit.Test;

public class PutMapAssocTest extends AbstractOpTest {

    private static final Atom ok = Atom.of("ok");
    private static final Atom put_map_assoc = Atom.of("put_map_assoc");
    private static final Atom test = Atom.of("test");

    public PutMapAssocTest() {
        super("put_map_assoc.beam");
    }

    @Test
    public void test_put_map_assoc() throws ThrowException {
        List params = List.of(new Map());
        Term result = Erlang.apply(put_map_assoc, test, params);
        assertEquals(ok, result);
    }

}
