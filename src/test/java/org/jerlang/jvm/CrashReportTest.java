package org.jerlang.jvm;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;

import org.jerlang.erts.Erlang;
import org.jerlang.type.Atom;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Str;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;
import org.jerlang.vm.VirtualMachine;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * Runs crash_demo.beam, compiled from src/test/erl/crash_demo.erl by
 * Erlang/OTP 29, and checks the error report an uncaught exception prints.
 */
public class CrashReportTest {

    private static final Path SOURCE = Paths.get("src/test/resources/crash_demo.beam");
    private static final Path LOCAL = Paths.get("crash_demo.beam");

    private String stdout;
    private String stderr;

    @Before
    public void start() throws Exception {
        Files.copy(SOURCE, LOCAL, StandardCopyOption.REPLACE_EXISTING);
        VirtualMachine.instance().start();
    }

    @After
    public void stop() throws Exception {
        VirtualMachine.instance().shutdown();
        Files.deleteIfExists(LOCAL);
    }

    @Test
    public void anUncaughtErrorPrintsTheReportWithItsStackFrame() throws Throwable {
        run("crash");

        assertTrue(stdout, stdout.matches(
            "(?s)=ERROR REPORT==== \\d{4}-\\d{2}-\\d{2}::\\d{2}:\\d{2}:\\d{2}\\.\\d{6} ===\n"
                + "Error in process <0\\.\\d+\\.\\d+> with exit value:\n.*"));
        assertTrue(stdout, stdout.contains("{{badmatch,2},"
            + "[{crash_demo,crash_now,1,[{file,\"crash_demo.erl\"},{line,19}]}]}"));
        assertTrue(stdout, stdout.contains("Process state:\n"));
        assertTrue(stdout, stdout.contains("  pid: <0."));
        assertTrue(stdout, stdout.contains("  reductions: "));
        assertTrue(stdout, stdout.contains("  mailbox: "));
        assertTrue(stdout, stdout.contains("  links: "));
        assertEquals(1, occurrences(stdout, "=ERROR REPORT===="));
        assertFalse(stderr, stderr.contains("=ERROR REPORT===="));
    }

    @Test
    public void theStacktraceContainsEveryCallerWithLineNumbers() throws Throwable {
        run("nested_crash");

        assertTrue(stdout, stdout.contains("{inner_boom,["
            + "{crash_demo,inner,1,[{file,\"crash_demo.erl\"},{line,34}]},"
            + "{crash_demo,outer,1,[{file,\"crash_demo.erl\"},{line,28}]}]}"));
    }

    @Test
    public void anUncaughtThrowIsReportedAsNocatch() throws Throwable {
        run("throw_crash");

        assertTrue(stdout, stdout.contains("{{nocatch,oops},"
            + "[{crash_demo,throw_now,0,[{file,\"crash_demo.erl\"},{line,43}]}]}"));
    }

    @Test
    public void anUncaughtExitIsNotACrash() throws Throwable {
        run("exit_crash");

        assertFalse(stdout, stdout.contains("=ERROR REPORT===="));
        assertFalse(stdout, stdout.contains("with exit value:"));
    }

    @Test
    public void aCaughtErrorPrintsNothing() throws Throwable {
        run("caught");

        assertFalse(stdout, stdout.contains("=ERROR REPORT===="));
        assertFalse(stdout, stdout.contains("with exit value:"));
    }

    @Test
    public void aMonitorReceivesTheErrorAndItsStacktrace() throws Throwable {
        Tuple reason = run("down_reason", Atom.of("error")).toTuple();

        assertEquals(Tuple.of(Atom.of("badmatch"), Integer.of(2)), reason.element(1));
        assertFirstFrame(reason.element(2), Atom.of("crash_now"), 1, 19);
    }

    @Test
    public void aMonitorReceivesNocatchAndItsStacktraceForAThrow() throws Throwable {
        Tuple reason = run("down_reason", Atom.of("throw")).toTuple();

        assertEquals(Tuple.of(Atom.of("nocatch"), Atom.of("oops")), reason.element(1));
        assertFirstFrame(reason.element(2), Atom.of("throw_now"), 0, 43);
    }

    @Test
    public void aMonitorReceivesAPlainReasonForExit() throws Throwable {
        assertEquals(Atom.of("bye"), run("down_reason", Atom.of("exit")));
    }

    @Test
    public void aLinkedProcessReceivesTheErrorAndItsStacktrace() throws Throwable {
        Tuple reason = run("linked_reason").toTuple();

        assertEquals(Tuple.of(Atom.of("badmatch"), Integer.of(2)), reason.element(1));
        assertFirstFrame(reason.element(2), Atom.of("crash_now"), 1, 19);
    }

    private static void assertFirstFrame(Term stacktrace, Atom function, int arity, int line) {
        List frames = stacktrace.toList();
        assertFalse("empty stacktrace", frames.length() == 0);
        Tuple frame = frames.head().toTuple();
        assertEquals(Atom.of("crash_demo"), frame.element(1));
        assertEquals(function, frame.element(2));
        assertEquals(Integer.of(arity), frame.element(3));
        List location = frame.element(4).toList();
        assertEquals(Tuple.of(Atom.of("file"), Str.of("crash_demo.erl")), location.head());
        assertEquals(Tuple.of(Atom.of("line"), Integer.of(line)), location.tail().head());
    }

    private Term run(String function, Term... args) throws Throwable {
        PrintStream originalOut = System.out;
        PrintStream originalErr = System.err;
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        ByteArrayOutputStream err = new ByteArrayOutputStream();
        PrintStream capturedOut = new PrintStream(out, true, "UTF-8");
        PrintStream capturedErr = new PrintStream(err, true, "UTF-8");
        System.setOut(capturedOut);
        System.setErr(capturedErr);
        Term result;
        try {
            result = Erlang.apply(Atom.of("crash_demo"), Atom.of(function), List.of(args));
        } finally {
            capturedOut.flush();
            capturedErr.flush();
            System.setOut(originalOut);
            System.setErr(originalErr);
            stdout = new String(out.toByteArray(), StandardCharsets.UTF_8)
                .replace("\r\n", "\n");
            stderr = new String(err.toByteArray(), StandardCharsets.UTF_8)
                .replace("\r\n", "\n");
        }
        return result;
    }

    private static int occurrences(String text, String needle) {
        int count = 0;
        int index = text.indexOf(needle);
        while (index >= 0) {
            count++;
            index = text.indexOf(needle, index + needle.length());
        }
        return count;
    }

}
