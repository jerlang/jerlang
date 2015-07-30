package org.jerlang.stdlib;

import org.jerlang.stdlib.shell.ShellHistory;
import org.jerlang.stdlib.shell.ShellStart;
import org.jerlang.type.Integer;

/**
 * = shell
 *
 * The Erlang Shell
 *
 * http://www.erlang.org/doc/man/shell.html
 */
public class Shell {

    public static void start() {
        ShellStart.start_0();
    }

    public static Integer history(Integer n) {
        return ShellHistory.history_1(n);
    }

}
