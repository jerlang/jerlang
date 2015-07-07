package org.jerlang;

import static org.junit.Assert.assertEquals;

import org.jerlang.type.Atom;
import org.jerlang.type.Integer;
import org.junit.Test;

public class FunctionSignatureTest {

    @Test
    public void testToString() {
        Atom mod = new Atom("mod");
        Atom fun = new Atom("fun");
        Integer arity = new Integer(0);
        FunctionSignature functionSignature = new FunctionSignature(mod, fun, arity);
        assertEquals("mod:fun/0", functionSignature.toString());
    }

}
