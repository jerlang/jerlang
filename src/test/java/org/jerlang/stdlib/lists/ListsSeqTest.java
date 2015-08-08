package org.jerlang.stdlib.lists;

import static org.junit.Assert.assertEquals;

import org.jerlang.stdlib.Lists;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.junit.Test;

public class ListsSeqTest {

    @Test
    public void testSeq2() {
        assertEquals(make_list(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), Lists.seq(Integer.of(1), Integer.of(10)));
    }

    @Test
    public void testSeq3() {
        assertEquals(make_list(1, 4, 7, 10, 13, 16, 19), Lists.seq(Integer.of(1), Integer.of(20), Integer.of(3)));
    }

    private List make_list(int... values) {
        List result = List.nil;
        for (int i = values.length - 1; i >= 0; i--) {
            result = new List(Integer.of(values[i]), result);
        }
        return result;
    }

}
