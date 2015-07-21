package org.jerlang.erts;

import java.util.HashMap;

import org.jerlang.erts.emulator.EmulatorFlag;
import org.jerlang.erts.emulator.InitFlag;
import org.jerlang.erts.erlang.Error;
import org.jerlang.stdlib.Lists;
import org.jerlang.type.Atom;
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
    }

    public static void parse(String[] args) {
        for (int index = 0; index < args.length; index++) {
            String arg = args[index];
            if (InitFlag.valid(arg)) {
                // TODO
            } else if (EmulatorFlag.valid(arg)) {
                // TODO
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
}
