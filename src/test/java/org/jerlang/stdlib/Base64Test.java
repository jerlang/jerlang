package org.jerlang.stdlib;

import static org.jerlang.erts.Erlang.apply;
import static org.jerlang.type.List.nil;
import static org.junit.Assert.assertEquals;

import org.jerlang.erts.emulator.op.AbstractOpTest;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.Atom;
import org.jerlang.type.Binary;
import org.jerlang.type.Str;
import org.jerlang.type.Term;
import org.junit.Test;

public class Base64Test extends AbstractOpTest {

    private static final Atom base64test = Atom.of("base64test");
    private static final Atom test1 = Atom.of("test1");
    private static final Atom test2 = Atom.of("test2");

    public Base64Test() {
        super("base64test.beam");
    }

    @Test
    public void test1() throws ThrowException {
        Binary expected = new Binary(new byte[] { 65, 65, 69, 67, 65, 119, 81, 70, 66, 103, 99, 73, 67, 81, 61, 61 });
        Term result = apply(base64test, test1, nil);
        assertEquals(expected, result);
    }

    @Test
    public void test2() throws ThrowException {
        Str expected = Str.of("AAECAwQFBgcICQ==");
        Term result = apply(base64test, test2, nil);
        assertEquals(expected, result);
    }

}
