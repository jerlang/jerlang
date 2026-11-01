package org.jerlang.jvm;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;

import org.jerlang.stdlib.Logger;
import org.jerlang.stdlib.Maps;
import org.jerlang.type.Atom;
import org.jerlang.type.Str;
import org.jerlang.type.Tuple;
import org.junit.After;
import org.junit.Test;

/**
 * The logger from docs/operations.md: OTP reports are java.util.logging
 * records, the OTP level is on the record, and the logger's level filters both
 * the reports and logger:allow/2.
 */
public class LoggerHandlerTest {

    private final List<LogRecord> records = new ArrayList<>();

    private final Handler handler = new Handler() {
        @Override
        public void publish(LogRecord record) {
            records.add(record);
        }

        @Override
        public void flush() {
        }

        @Override
        public void close() {
        }
    };

    @After
    public void restore() {
        Logger.jul().removeHandler(handler);
        Logger.jul().setLevel(Level.ALL);
        records.clear();
    }

    @Test
    public void aReportReachesHandlersWithItsOtpLevel() {
        Logger.jul().addHandler(handler);

        Logger.log(Atom.of("warning"),
            Tuple.of(Atom.of("report"), Str.of("disk almost full")), Maps._new());

        assertEquals(1, records.size());
        Logger.Event event = (Logger.Event) records.get(0);
        assertEquals(Atom.of("warning"), event.otpLevel());
        assertTrue(event.getMessage(), event.getMessage().contains("=WARNING REPORT===="));
        assertTrue(event.getMessage(), event.getMessage().contains("disk almost full"));
    }

    @Test
    public void theLoggerLevelFiltersReportsAndAllow() {
        Logger.jul().addHandler(handler);
        Logger.jul().setLevel(Level.WARNING);

        assertEquals("false", Logger.allow(Atom.of("info"), Atom.of("a_module")).toString());
        Logger.log(Atom.of("info"), Str.of("quiet"));
        assertTrue("info is below the logger level", records.isEmpty());

        assertEquals("true", Logger.allow(Atom.of("error"), Atom.of("a_module")).toString());
        Logger.log(Atom.of("error"), Str.of("loud"));
        assertEquals(1, records.size());
        assertEquals("ERROR", records.get(0).getLevel().getName());
    }

}
