package org.jerlang.erts;

import java.util.HashMap;

import org.jerlang.FunctionSignature;
import org.jerlang.Process;
import org.jerlang.stdlib.Lists;
import org.jerlang.type.Atom;
import org.jerlang.type.List;
import org.jerlang.type.Str;

/**
 * The runtime holds the global states of this node.
 */
public class Runtime {

    private static boolean extraFlag = false;

    // The function to invoke after node has started
    private static FunctionSignature runFlag = null;

    private static List plainArguments = List.nil;
    private static final ThreadLocal<org.jerlang.Process> process = new ThreadLocal<>();
    private static final HashMap<Atom, List> userFlags = new HashMap<>();

    public static void addPlainArgument(String string) {
        plainArguments = new List(Str.of(string), plainArguments);
    }

    public static boolean extraFlag() {
        return extraFlag;
    }

    public static HashMap<Atom, List> userFlags() {
        return userFlags;
    }

    public static org.jerlang.Process getProcess() {
        return process.get();
    }

    public static FunctionSignature getRunFlag() {
        return runFlag;
    }

    public static List plainArguments() {
        return Lists.reverse_1(plainArguments);
    }

    /**
     * Used by unit tests only.
     * Resets all states back to their default.
     */
    public static void reset() {
        extraFlag = false;
    }

    public static void setExtraFlag() {
        extraFlag = true;
    }

    public static void setProcess(Process proc) {
        process.set(proc);
    }

    public static void setRunFlag(FunctionSignature signature) {
        runFlag = signature;
    }

}
