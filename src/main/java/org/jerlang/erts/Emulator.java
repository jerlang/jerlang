package org.jerlang.erts;

import java.util.HashMap;

import org.jerlang.FunctionSignature;
import org.jerlang.erts.emulator.EmulatorFlag;
import org.jerlang.erts.emulator.InitFlag;
import org.jerlang.erts.erlang.Error;
import org.jerlang.stdlib.Lists;
import org.jerlang.stdlib.Shell;
import org.jerlang.type.Atom;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Str;

/**
 * The erl program starts an Erlang runtime system.
 *
 * http://erlang.org/doc/man/erl.html
 */
public class Emulator {

    private static final Atom unsupported_option = Atom.of("unsupported_option");
    private static final HashMap<Atom, List> userFlags = Runtime.userFlags();

    public static void main(String[] args) {
        parse(args);
        Shell.start();
    }

    public static void parse(String[] args) {
        for (int index = 0; index < args.length; index++) {
            String arg = args[index];
            if (Runtime.extraFlag()) {
                Runtime.addPlainArgument(arg);
            } else if (InitFlag.valid(arg)) {
                index = parseInitFlag(InitFlag.of(arg), args, index);
            } else if (EmulatorFlag.valid(arg)) {
                index = parseEmulatorFlag(EmulatorFlag.of(arg), args, index);
            } else {
                // User flag
                if (!arg.startsWith("-") || arg.length() < 2) {
                    throw new Error("Invalid user flag: " + arg);
                }
                Atom userFlag = Atom.of(arg.substring(1));
                List flagList = List.nil;
                while (index + 1 < args.length && !args[index + 1].startsWith("-")) {
                    flagList = new List(Str.of(args[++index]), flagList);
                }
                flagList = Lists.reverse(flagList);
                if (!userFlags.containsKey(userFlag)) {
                    userFlags.put(userFlag, List.nil);
                }
                List newList = Lists.reverse(new List(flagList, userFlags.get(userFlag)));
                userFlags.put(userFlag, newList);
            }
        }
    }

    private static int parseEmulatorFlag(EmulatorFlag flag, String[] args, int index) {
        switch (flag) {
        case RELEASE:
        case WARNING:
            // TODO
            return index + 1;
        default:
            throw new Error("Unsupported emulator flag: " + flag);
        }
    }

    private static int parseInitFlag(InitFlag flag, String[] args, int index) {
        switch (flag) {
        case EXTRA:
            Runtime.setExtraFlag();
            return index;
        case RUN:
        case S:
            return parseRunFlag(args, index);
        case SNAME:
            // TODO
            return index + 1;
        default:
            throw new Error("Unsupported init flag: " + flag);
        }
    }

    private static int parseRunFlag(String[] args, int index) {
        Atom mod = Atom.of(args[++index]);
        Atom func = Atom.of("start");
        if (index < args.length) {
            if (args[index + 1].startsWith("-")) {
                Runtime.setRunFlag(new FunctionSignature(mod, func, Integer.of(0)));
                return index;
            }
            func = Atom.of(args[++index]);
        }
        List arguments = List.nil;
        while (++index < args.length) {
            arguments = new List(Str.of(args[index]), arguments);
        }
        arguments = Lists.reverse(arguments);
        Integer arity = Erlang.length(arguments);
        Runtime.setRunFlag(new FunctionSignature(mod, func, arity));
        return index;
    }

}
