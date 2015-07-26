package org.jerlang.erts;

import static org.jerlang.erts.Erlang.process_flag;
import static org.jerlang.erts.Erlang.register;
import static org.jerlang.erts.Erlang.self;
import static org.jerlang.erts.init.Boot.boot3;
import static org.jerlang.erts.init.Boot.flags_to_atoms_again;
import static org.jerlang.erts.init.Boot.start_on_load_handler_process;

import org.jerlang.erts.erlang.Error;
import org.jerlang.erts.init.Boot;
import org.jerlang.stdlib.Maps;
import org.jerlang.type.Atom;
import org.jerlang.type.List;
import org.jerlang.type.Map;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;

/**
 * http://www.erlang.org/doc/man/init.html
 * https://github.com/erlang/otp/blob/master/erts/preloaded/src/init.erl
 */
public class Init {

    public static final String[] EXPORT = {
        "boot/1",
        "get_argument/1",
        "get_arguments/0",
        "get_plain_arguments/0"
    };

    private static final Atom error = Atom.of("error");
    private static final Atom init = Atom.of("init");
    private static final Atom ok = Atom.of("ok");
    private static final Atom trap_exit = Atom.of("trap_exit");

    /**
     * boot(BootArgs) -> no_return()
     *
     * Types:
     * BootArgs = [binary()]
     *
     * Starts the Erlang runtime system.
     * This function is called when the emulator is started and
     * coordinates system start-up.
     *
     * BootArgs are all command line arguments except the emulator flags,
     * that is, flags and plain arguments. See erl(1).
     *
     * init itself interprets some of the flags, see Command Line Flags below.
     * The remaining flags ("user flags") and plain arguments are passed to the
     * init loop and can be retrieved by calling get_arguments/0 and
     * get_plain_arguments/0, respectively.
     *
     * http://www.erlang.org/doc/man/init.html#boot-1
     */
    public static Term boot(List params) {
        switch (Erlang.length_1(params).toInt()) {
        case 1:
            boot_1(params.head().toList());
            break;
        default:
            throw new Error("badarg");
        }
        return null;
    }

    static void boot_1(List bootArgs) {
        // called by OtpRing0 module
        register(init, self());
        process_flag(trap_exit, true);
        start_on_load_handler_process();
        Tuple tuple = Boot.parse_boot_args(bootArgs);
        Map start0 = (Map) tuple.element(0);
        List flags = (List) tuple.element(1);
        Map args = (Map) tuple.element(2);
        Map start = Maps.map(null, start0);
        List flags0 = flags_to_atoms_again(flags);
        boot3(start, flags0, args);
    }

    /**
     * Returns all values associated with the command line user flag Flag.
     * If Flag is provided several times, each Values is returned in
     * preserved order.
     *
     * There are also a number of flags, which are defined automatically
     * and can be retrieved using this function:
     *
     * `root`::
     * The installation directory of Erlang/OTP, `$ROOT`.
     *
     * `progname`::
     * The name of the program which started Erlang.
     *
     * `home`::
     * The home directory.
     *
     * Returns `error` if there is no value associated with `flag`.
     */
    public static Term get_argument(List params) {
        switch (Erlang.length_1(params).toInt()) {
        case 0:
            return get_argument_1(params.head().toAtom());
        default:
            throw new Error("badarg");
        }
    }

    public static Term get_argument_1(Atom flag) {
        if (Runtime.userFlags().containsKey(flag)) {
            return Tuple.of(ok, Runtime.userFlags().get(flag));
        } else {
            return error;
        }
    }

    /**
     * Returns all command line flags, as well as the system defined flags,
     * see get_argument/1.
     */
    public static Term get_arguments(List params) {
        switch (Erlang.length_1(params).toInt()) {
        case 0:
            return get_arguments_0();
        default:
            throw new Error("badarg");
        }
    }

    public static List get_arguments_0() {
        return List.nil;
    }

    /**
     * Returns any plain command line arguments as a list of strings.
     */
    public static Term get_plain_arguments(List params) {
        switch (Erlang.length_1(params).toInt()) {
        case 0:
            return get_plain_arguments_0();
        default:
            throw new Error("badarg");
        }
    }

    public static List get_plain_arguments_0() {
        return Runtime.plainArguments();
    }

    public static void stop() {
        System.exit(0);
    }

}
