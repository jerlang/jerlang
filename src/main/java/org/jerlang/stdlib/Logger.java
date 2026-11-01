package org.jerlang.stdlib;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;

import org.jerlang.type.Atom;
import org.jerlang.type.Boolean;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;

/**
 * A small stand-in for OTP's logger, backed by java.util.logging.
 *
 * The real logger needs the kernel application: its tables are created by the
 * application controller and its configuration comes from the application
 * environment. A runtime that does not boot applications can not run it, but
 * OTP's own library modules log through it. The ?LOG_ERROR macro of
 * logger.hrl, which the supervisor and gen_server use, expands to
 * `logger:allow/2` followed by `logger:log/3`; without a working logger a
 * child report raises a second exception and takes the supervisor down.
 *
 * So this module answers the calls and reports them through the
 * java.util.logging logger {@value #JUL_LOGGER}. The OTP levels are mapped onto
 * java.util.logging levels, `logger:allow/2,3` filters on them, and every
 * report reaches handlers as a {@link Logger.Event} that carries the original
 * OTP level and the caller's metadata. Filters, handlers and formatters given
 * to {@code logger:add_handler/3} are still accepted and ignored: the OTP
 * handler chain is not implemented.
 *
 * A default handler prints reports in the shape of the OTP default handler, so
 * a program that does not configure logging sees what it saw before. An
 * embedding deployment that wants its own sink takes the logger from
 * {@link #jul()} and replaces or adds handlers, or sets its level:
 *
 * ----
 * Logger.jul().setLevel(Level.WARNING);
 * Logger.jul().addHandler(myHandler);
 * ----
 *
 * http://www.erlang.org/doc/man/logger.html
 */
public class Logger {

    /** The java.util.logging logger OTP style reports go through. */
    public static final String JUL_LOGGER = "org.jerlang.otp";

    private static final DateTimeFormatter TIMESTAMP =
        DateTimeFormatter.ofPattern("yyyy-MM-dd::HH:mm:ss.SSSSSS");

    private static final Atom OK = Atom.of("ok");
    private static final Atom MSG = Atom.of("msg");

    // OTP's levels, lowest first.
    private static final java.util.List<String> NAMES = java.util.Arrays.asList(
        "debug", "info", "notice", "warning", "error", "critical", "alert", "emergency");

    private static final Level NOTICE = new Level("NOTICE", 850) { };
    private static final Level ERROR = new Level("ERROR", 950) { };
    private static final Level CRITICAL = new Level("CRITICAL", 1000) { };
    private static final Level ALERT = new Level("ALERT", 1050) { };
    private static final Level EMERGENCY = new Level("EMERGENCY", 1100) { };

    private static final Level[] LEVELS = {
        Level.FINE, Level.INFO, NOTICE, Level.WARNING, ERROR, CRITICAL, ALERT, EMERGENCY
    };

    private static final java.util.logging.Logger JUL =
        java.util.logging.Logger.getLogger(JUL_LOGGER);

    static {
        JUL.setUseParentHandlers(false);
        JUL.setLevel(Level.ALL);
        JUL.addHandler(new OtpHandler());
    }

    /**
     * A report in flight, with the OTP level and metadata a handler needs.
     */
    public static final class Event extends LogRecord {

        private final Atom otpLevel;
        private final Term metadata;

        private Event(Level level, Atom otpLevel, Term metadata, String text) {
            super(level, text);
            this.otpLevel = otpLevel;
            this.metadata = metadata;
        }

        /** The OTP level atom, for example `warning`. */
        public Atom otpLevel() {
            return otpLevel;
        }

        /** The metadata map the caller passed, or the empty map. */
        public Term metadata() {
            return metadata;
        }

    }

    /**
     * The java.util.logging logger reports go through, so a deployment can
     * set its level or install handlers.
     */
    public static java.util.logging.Logger jul() {
        return JUL;
    }

    public static Term log(Term level, Term report) {
        return log(level, report, Maps._new());
    }

    public static Term log(Term level, Term report, Term metadata) {
        Atom name = level instanceof Atom ? (Atom) level : Atom.of(level.toString());
        Level julLevel = level(name);
        if (!JUL.isLoggable(julLevel)) {
            return OK;
        }
        Event event = new Event(julLevel, name, metadata, text(name, report));
        source(event, metadata);
        JUL.log(event);
        return OK;
    }

    /**
     * The form the ?LOG_* macros of logger.hrl use:
     * logger:macro_log(Level, Report, Meta, Module).
     */
    public static Term macro_log(Term level, Term report, Term metadata, Term module) {
        return log(level, report, metadata);
    }

    /**
     * Whether a report at this level would be handled, which is what
     * java.util.logging calls isLoggable.
     */
    public static Term allow(Term level, Term module) {
        return allowed(level instanceof Atom ? (Atom) level : Atom.of(level.toString()));
    }

    public static Term allow(Term level, Term module, Term metadata) {
        return allowed(level instanceof Atom ? (Atom) level : Atom.of(level.toString()));
    }

    /**
     * The comparison the logger uses to filter events: lt, eq or gt.
     */
    public static Term compare_levels(Term first, Term second) {
        int a = NAMES.indexOf(first.toString());
        int b = NAMES.indexOf(second.toString());
        if (a < 0 || b < 0) {
            throw org.jerlang.erts.erlang.Error.badarg;
        }
        return Atom.of(a < b ? "lt" : a == b ? "eq" : "gt");
    }

    private static Term allowed(Atom name) {
        return JUL.isLoggable(level(name)) ? Boolean.am_true : Boolean.am_false;
    }

    private static Level level(Atom name) {
        int index = NAMES.indexOf(name.toString());
        return index < 0 ? Level.INFO : LEVELS[index];
    }

    /**
     * The text of a report in the shape of the OTP default handler.
     */
    private static String text(Atom level, Term report) {
        StringBuilder text = new StringBuilder();
        text.append('=').append(level.toString().toUpperCase()).append(" REPORT==== ")
            .append(LocalDateTime.now().format(TIMESTAMP)).append(" ===\n");
        text.append(message(report));
        return text.toString();
    }

    /**
     * The metadata a formatter would use: the source class and function, when
     * the caller supplied them.
     */
    private static void source(Event event, Term metadata) {
        if (!(metadata instanceof org.jerlang.type.Map)) {
            return;
        }
        org.jerlang.type.Map map = (org.jerlang.type.Map) metadata;
        Term module = map.is_key(Atom.of("module")) ? map.get(Atom.of("module")) : null;
        if (module != null) {
            event.setSourceClassName(module.toString());
        }
        Term mfa = map.is_key(Atom.of("mfa")) ? map.get(Atom.of("mfa")) : null;
        if (mfa != null) {
            event.setSourceMethodName(mfa.toString());
        }
    }

    /**
     * The message of a report, without the metadata a formatter would use.
     */
    private static String message(Term report) {
        Term value = report;
        if (value instanceof Tuple && value.toTuple().arity() == 2) {
            Term tag = value.toTuple().element(1);
            if (tag.toString().equals("report") || tag.toString().equals("string")) {
                value = value.toTuple().element(2);
            }
        }
        if (value instanceof org.jerlang.type.Map && value.toMap().is_key(MSG)) {
            return message(value.toMap().get(MSG));
        }
        return String.valueOf(value);
    }

    /**
     * The default handler: prints reports the way OTP's default handler does,
     * one report per write so reports from different schedulers do not
     * interleave line by line.
     */
    private static final class OtpHandler extends Handler {

        @Override
        public void publish(LogRecord record) {
            String text = record.getMessage();
            if (text == null) {
                return;
            }
            System.out.println(text);
            System.out.flush();
        }

        @Override
        public void flush() {
        }

        @Override
        public void close() {
        }

    }

}
