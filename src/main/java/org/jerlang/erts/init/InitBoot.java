package org.jerlang.erts.init;

import static org.jerlang.erts.Erlang.process_flag;
import static org.jerlang.erts.Erlang.register;
import static org.jerlang.erts.Erlang.self;

import org.jerlang.erts.erlang.Error;
import org.jerlang.stdlib.Maps;
import org.jerlang.type.Atom;
import org.jerlang.type.Boolean;
import org.jerlang.type.List;
import org.jerlang.type.Map;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;

public class InitBoot {

    private static final Atom init = Atom.of("init");
    private static final Atom trap_exit = Atom.of("trap_exit");

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 1:
            boot_1(params.head().toList());
            return null;
        default:
            throw Error.badarg;
        }
    }

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
    public static void boot_1(List bootArgs) {
        // called by OtpRing0 module
        register(init, self());
        process_flag(trap_exit, Boolean.am_true);
        start_on_load_handler_process();
        Tuple tuple = parse_boot_args(bootArgs);
        Map start0 = (Map) tuple.element(0);
        List flags = (List) tuple.element(1);
        Map args = (Map) tuple.element(2);
        Map start = Maps.map(null, start0);
        List flags0 = flags_to_atoms_again(flags);
        boot3(start, flags0, args);
    }

    private static final Atom init__boot__on_load_handler = Atom.of("init__boot__on_load_handler");
    private static final Atom ON_LOAD_HANDLER = init__boot__on_load_handler;

    /**
     *
     */
    public static void boot3(Object start, Object flags, Object args) {
        /*
        boot(Start,Flags,Args) ->
        BootPid = do_boot(Flags,Start),
        ProcessState = #state{flags = Flags,
           args = Args,
           start = Start,
           bootpid = BootPid},
        boot_loop(BootPid,ProcessState).
        */
    }

    public static void start_on_load_handler_process() {
        //register(ON_LOAD_HANDLER, spawn(Str.of("on_load_handler_init/0")));
    }

    public static Tuple parse_boot_args(Object args) {
        /*
        parse_boot_args(Args) ->
        parse_boot_args(Args, [], [], []).

        parse_boot_args([B|Bs], Ss, Fs, As) ->
        case check(B) of
        start_extra_arg ->
            {reverse(Ss),reverse(Fs),lists:reverse(As, Bs)}; % BIF
        start_arg ->
            {S,Rest} = get_args(Bs, []),
            parse_boot_args(Rest, [{s, S}|Ss], Fs, As);
        start_arg2 ->
            {S,Rest} = get_args(Bs, []),
            parse_boot_args(Rest, [{run, S}|Ss], Fs, As);
        eval_arg ->
            {Expr,Rest} = get_args(Bs, []),
            parse_boot_args(Rest, [{eval, Expr}|Ss], Fs, As);
        flag ->
            {F,Rest} = get_args(Bs, []),
            Fl = case F of
                 []   -> [B];
                 FF   -> [B,FF]
             end,
            parse_boot_args(Rest, Ss,
                    [list_to_tuple(Fl)|Fs], As);
        arg ->
            parse_boot_args(Bs, Ss, Fs, [B|As]);
        end_args ->
            parse_boot_args(Bs, Ss, Fs, As)
        end;
        parse_boot_args([], Start, Flags, Args) ->
        {reverse(Start),reverse(Flags),reverse(Args)}.
        */
        return Tuple.of(List.nil, List.nil, List.nil); // TODO
    }

    public static List flags_to_atoms_again(List flags) {
        if (flags.length() == 0) {
            return flags;
        }
        List newList = null;
        while (flags.head() != null) {
            Tuple tuple = (Tuple) flags.head();
            switch (tuple.arity()) {
            case 1:
                newList = new List(b2a(tuple.element(0)), newList);
                break;
            case 2:
                newList = new List(Tuple.of(b2a(tuple.element(0)), tuple.element(1)), newList);
                break;
            default:
                // throw error
            }
        }
        return newList;
    }

    private static Atom b2a(Term term) {
        if (term instanceof Atom) {
            return (Atom) term;
        }
        // TODO: binary_to_list -> list_to_atom
        return Atom.of("TODO");
    }

}
