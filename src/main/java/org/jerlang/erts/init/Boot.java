package org.jerlang.erts.init;

import static org.jerlang.erts.Erlang.register;
import static org.jerlang.erts.Erlang.spawn;

import org.jerlang.erts.Erlang;
import org.jerlang.type.Atom;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;

public class Boot {

    private static final Atom init__boot__on_load_handler = Atom.of("init__boot__on_load_handler");
    private static final Atom ON_LOAD_HANDLER = init__boot__on_load_handler;

    /**
     *
     */
    public static void boot3(Object start, Object flags, Object args) {
        /*
        boot(Start,Flags,Args) ->
        BootPid = do_boot(Flags,Start),
        State = #state{flags = Flags,
           args = Args,
           start = Start,
           bootpid = BootPid},
        boot_loop(BootPid,State).
        */
    }

    public static void start_on_load_handler_process() {
        register(ON_LOAD_HANDLER, spawn("on_load_handler_init/0"));
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
        if (Erlang.length_1(flags).equals(Integer.of(0))) {
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
