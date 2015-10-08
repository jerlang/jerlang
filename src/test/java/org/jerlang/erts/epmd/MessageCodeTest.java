package org.jerlang.erts.epmd;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import org.junit.Test;

public class MessageCodeTest {

    @Test
    public void testMessageCodes() {
        assertEquals(100, MessageCode.DUMP_REQ.toCode());
        assertEquals(107, MessageCode.KILL_REQ.toCode());
        assertEquals(110, MessageCode.NAMES_REQ.toCode());
        assertEquals(115, MessageCode.STOP_REQ.toCode());
        assertEquals(119, MessageCode.PORT2_RESP.toCode());
        assertEquals(120, MessageCode.ALIVE2_REQ.toCode());
        assertEquals(121, MessageCode.ALIVE2_RESP.toCode());
        assertEquals(122, MessageCode.PORT2_REQ.toCode());
    }

    @Test
    public void testByCode() {
        assertNull(MessageCode.byCode(0));
        assertNull(MessageCode.byCode(99));
        assertNull(MessageCode.byCode(123));
        assertEquals(MessageCode.DUMP_REQ, MessageCode.byCode(100));
        assertEquals(MessageCode.KILL_REQ, MessageCode.byCode(107));
        assertEquals(MessageCode.NAMES_REQ, MessageCode.byCode(110));
        assertEquals(MessageCode.STOP_REQ, MessageCode.byCode(115));
        assertEquals(MessageCode.PORT2_RESP, MessageCode.byCode(119));
        assertEquals(MessageCode.ALIVE2_REQ, MessageCode.byCode(120));
        assertEquals(MessageCode.ALIVE2_RESP, MessageCode.byCode(121));
        assertEquals(MessageCode.PORT2_REQ, MessageCode.byCode(122));
    }

}
