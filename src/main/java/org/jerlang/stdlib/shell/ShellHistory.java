package org.jerlang.stdlib.shell;

import org.jerlang.erts.Runtime;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;

public class ShellHistory {

    private ShellHistory() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 1:
            return history_1(params.head().toInteger());
        default:
            throw new Error("badarg");
        }
    }

    /**
     * Sets the number of previous commands to keep in the history list to N.
     * The previous number is returned.
     * The default number is 20.
     *
     * http://www.erlang.org/doc/man/shell.html#history-1
     */
    public static Integer history_1(Integer n) {
        Integer previous = Integer.of(Runtime.shellHistory().getMaxSize());
        Runtime.shellHistory().setMaxSize(n.toInt());
        return previous;
    }

}
