package org.jerlang.stdlib.lists;

import static org.junit.Assert.assertEquals;

import org.jerlang.stdlib.Lists;
import org.jerlang.type.Atom;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Str;
import org.junit.Test;

public class ListsConcatTest {

    @Test
    public void testConcat() {
        Atom doc = Atom.of("doc");
        Atom slash = Atom.of("/");
        Atom file = Atom.of("file");
        Atom dot = Atom.of(".");
        Integer three = Integer.of(3);

        List given = List.of(doc, slash, file, dot, three);
        List expected = Str.of("doc/file.3");
        assertEquals(expected, Lists.concat(given));
    }

}
