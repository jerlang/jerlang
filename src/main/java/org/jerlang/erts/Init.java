package org.jerlang.erts;

import org.jerlang.erts.init.InitBoot;
import org.jerlang.erts.init.InitGetArgument;
import org.jerlang.erts.init.InitGetArguments;
import org.jerlang.erts.init.InitGetPlainArguments;
import org.jerlang.erts.init.InitStop;
import org.jerlang.type.Atom;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * http://www.erlang.org/doc/man/init.html
 * https://github.com/erlang/otp/blob/master/erts/preloaded/src/init.erl
 */
public class Init {

    public static void boot(List bootArgs) {
        InitBoot.boot_1(bootArgs);
    }

    public static Term get_argument(Atom flag) {
        return InitGetArgument.get_argument_1(flag);
    }

    public static List get_arguments() {
        return InitGetArguments.get_arguments_0();
    }

    public static List get_plain_arguments() {
        return InitGetPlainArguments.get_plain_arguments_0();
    }

    public static void stop() {
        InitStop.stop_0();
    }

}
