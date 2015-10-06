package org.jerlang.erts.erlang;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;
import static org.junit.Assert.assertEquals;

import org.jerlang.AbstractVirtualMachineTest;
import org.jerlang.FunctionSignature;
import org.jerlang.Module;
import org.jerlang.ModuleRegistry;
import org.jerlang.erts.Erlang;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.Atom;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Str;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;
import org.junit.Test;

public class ErlangTest extends AbstractVirtualMachineTest {

    @Test
    public void testAbs() throws ThrowException {
        Module m = ModuleRegistry.get(Atom.of("erlang"));
        FunctionSignature s = new FunctionSignature(m, "abs", 1);
        Integer negative = Integer.of(-1);
        Term result = m.apply(s, List.of(negative));
        assertThat(result).isNotNull();
        Integer positive = result.toInteger();
        assertThat(positive.toInt()).isEqualTo(1);
    }

    @Test
    public void testLength1() {
        Integer zero = Integer.of(0);
        assertEquals(zero, Erlang.length(List.nil));
        assertEquals(zero, Erlang.length(List.of()));
        assertEquals(Integer.of(1), Erlang.length(List.of(zero)));
        assertEquals(Integer.of(2), Erlang.length(List.of(zero, zero)));
    }

    @Test
    public void testTupleSize() {
        Atom a = Atom.of("a");
        Atom b = Atom.of("b");
        Atom c = Atom.of("c");
        Tuple tuple = Tuple.of(a, b, c);
        assertThat(Erlang.tuple_size(tuple)).isEqualTo(Integer.of(3));
    }

    @Test
    public void testTupleToList() {
        Atom a = Atom.of("a");
        Tuple b = Tuple.of(Str.of("b"), Integer.of(123));
        Tuple tuple = Tuple.of(a, b);
        List list = Erlang.tuple_to_list(tuple);
        assertThat(list).isNotNull();
        assertThat(list.head()).isEqualTo(a);
        list = list.tail();
        assertThat(list.head()).isEqualTo(b);
        list = list.tail();
        assertThat(list.head()).isEqualTo(List.nil);
    }

}
