package org.jerlang.erts.emulator;

/**
 * Any argument starting with the character `-` (hyphen) is interpreted as
 * a flag which should be passed to the Erlang part of the runtime system,
 * more specifically to the init system process, see init(3).
 *
 * See:
 * http://www.erlang.org/doc/man/erl.html
 */
public enum InitFlag {

    /**
     * Everything following -- up to the next flag (-flag or +flag)
     * is considered plain arguments and can be retrieved using
     * init:get_plain_arguments/0.
     */
    DOUBLE_DASH("--"),

    /**
     * Sets the application configuration parameter Par to the
     * value Val for the application Application,
     * see app(4) and application(3).
     */
    APPLICATION("-Application"),

    /**
     *  Command line arguments are read from the file FileName.
     * The arguments read from the file replace the
     * '-args_file FileName' flag on the resulting command line.
     * The file FileName should be a plain text file and may
     * contain comments and command line arguments.
     * A comment begins with a # character and continues until
     * next end of line character.
     * Backslash (\\) is used as quoting character.
     * All command line arguments accepted by erl are allowed,
     * also the -args_file FileName flag.
     * Be careful not to cause circular dependencies between files
     * containing the -args_file flag, though.
     * The -extra flag is treated specially.
     * Its scope ends at the end of the file.
     * Arguments following an -extra flag are moved on the command
     * line into the -extra section, i.e. the end of the command
     * line following after an -extra flag.
     */
    ARGS_FILE("-args_file"),

    /**
     * The initial Erlang shell does not read user input until the
     * system boot procedure has been completed
     * (Erlang 5.4 and later).
     * This flag disables the start synchronization feature and
     * lets the shell start in parallel with the rest of the system.
     */
    ASYNC_SHELL_START("-async_shell_start"),

    /**
     * Specifies the name of the boot file, File.boot, which is
     * used to start the system. See init(3).
     * Unless File contains an absolute path, the system searches
     * for File.boot in the current and $ROOT/bin directories.
     * Defaults to $ROOT/bin/start.boot.
     */
    BOOT("-boot"),

    /**
     *  If the boot script contains a path variable Var other than
     * $ROOT, this variable is expanded to Dir.
     * Used when applications are installed in another directory
     * than $ROOT/lib, see systools:make_script/1,2.
     */
    BOOT_VAR("-boot_var"),

    /**
     * Enables the code path cache of the code server, see code(3).
     */
    CODE_PATH_CACHE("-code_path_cache"),

    /**
     * Compiles the specified modules and then terminates
     * (with non-zero exit code if the compilation of some
     * file did not succeed).
     * Implies -noinput. Not recommended - use erlc instead.
     */
    COMPILE("-compile"),

    /**
     * Specifies the name of a configuration file, Config.config,
     * which is used to configure applications.
     * See app(4) and application(3).
     */
    CONFIG("-config"),

    /**
     * If this flag is present, global will not maintain a fully
     * connected network of distributed Erlang nodes, and then
     * global name registration cannot be used. See global(3).
     */
    CONNECT_ALL("-connect_all"),

    /**
     * Obsolete flag without any effect and common misspelling
     * for -setcookie. Use -setcookie instead.
     */
    COOKIE("-cookie"),

    /**
     * Starts the Erlang runtime system detached from the system
     * console. Useful for running daemons and backgrounds processes.
     * Implies -noinput.
     */
    DETACHED("-detached"),

    /**
     * Useful for debugging.
     * Prints out the actual arguments sent to the emulator.
     */
    EMU_ARGS("-emu_args"),

    /**
     * Sets the host OS environment variable Variable to the value Value for
     * the Erlang runtime system. Example:
     *
     * `% erl -env DISPLAY gin:0`
     *
     * In this example, an Erlang runtime system is started with the DISPLAY
     * environment variable set to gin:0.
     */
    ENV("-env"),

    /**
     * Makes init evaluate the expression Expr, see init(3).
     */
    EVAL("-eval"),

    /**
     * Everything following -extra is considered plain arguments and can be
     * retrieved using `init:get_plain_arguments/0`.
     */
    EXTRA("-extra"),

    /**
     * Starts heart beat monitoring of the Erlang runtime system. See heart(3).
     */
    HEART("-heart"),

    /**
     * Starts the Erlang runtime system as a hidden node, if it is run as a
     * distributed node. Hidden nodes always establish hidden connections to
     * all other nodes except for nodes in the same global group.
     * Hidden connections are not published on either of the connected nodes,
     * i.e. neither of the connected nodes are part of the result from
     * `nodes/0` on the other node.
     * See also hidden global groups, global_group(3).
     */
    HIDDEN("-hidden"),

    /**
     * Specifies the IP addresses for the hosts on which Erlang boot servers
     * are running, see erl_boot_server(3). This flag is mandatory if the
     * -loader inet flag is present.
     *
     * The IP addresses must be given in the standard form (four decimal
     * numbers separated by periods, for example "150.236.20.74".
     * Hosts names are not acceptable, but a broadcast address
     * (preferably limited to the local network) is.
     */
    HOSTS("-hosts"),

    /**
     * Specifies the identity of the Erlang runtime system.
     * If it is run as a distributed node, Id must be identical to the name
     * supplied together with the -sname or -name flag.
     */
    ID("-id"),

    /**
     * Makes `init` write some debug information while interpreting the boot script.
     */
    INIT_DEBUG("-init_debug"),

    /**
     * Specifies the method used by `erl_prim_loader` to load Erlang modules
     * into the system. See erl_prim_loader(3).
     * Two Loader methods are supported, `efile` and `inet`. `efile` means use
     * the local file system, this is the default. `inet` means use a boot
     * server on another machine, and the `-id`, `-hosts` and `-setcookie`
     * flags must be specified as well. If Loader is something else, the user
     * supplied `Loader` port program is started.
     */
    LOADER("-loader"),

    /**
     * Makes the Erlang runtime system invoke `make:all()` in the current
     * working directory and then terminate. See make(3). Implies `-noinput`.
     */
    MAKE("-make"),

    /**
     * Displays the manual page for the Erlang module Module.
     * Only supported on Unix.
     */
    MAN("-man"),

    /**
     * Indicates if the system should load code dynamically (interactive),
     * or if all code should be loaded during system initialization (embedded),
     * see code(3). Defaults to interactive.
     */
    MODE("-mode"),

    /**
     * Makes the Erlang runtime system into a distributed node.
     * This flag invokes all network servers necessary for a node to become
     * distributed. See net_kernel(3). It is also ensured that epmd runs on
     * the current host before Erlang is started. See epmd(1).
     *
     * The name of the node will be Name@Host, where Host is the fully
     * qualified host name of the current host.
     * For short names, use the -sname flag instead.
     */
    NAME("-name"),

    /**
     * Ensures that the Erlang runtime system never tries to read any input.
     * Implies -noshell.
     */
    NOINPUT("-noinput"),

    /**
     * Starts an Erlang runtime system with no shell.
     * This flag makes it possible to have the Erlang runtime system
     * as a component in a series of UNIX pipes.
     */
    NOSHELL("-noshell"),

    /**
     * Disables the sticky directory facility of the Erlang code server.
     */
    NOSTICK("-nostick"),

    /**
     * Invokes the old Erlang shell from Erlang 3.3.
     * The old shell can still be used.
     */
    OLDSHELL("-oldshell"),

    /**
     * Adds the specified directories to the beginning of the code path,
     * similar to `code:add_pathsa/1`. See code(3).
     * As an alternative to `-pa`, if several directories are to be
     * prepended to the code and the directories have a common parent
     * directory, that parent directory could be specified in the
     * ERL_LIBS environment variable. See code(3).
     */
    PA("-pa"),

    /**
     * Adds the specified directories to the end of the code path,
     * similar to `code:add_pathsz/1`. See code(3).
     */
    PZ("-pz"),

    /**
     * Starts Erlang with a remote shell connected to Node.
     */
    REMSH("-remsh"),

    /**
     * Specifies an alternative to rsh for starting a slave node
     * on a remote host. See slave(3).
     */
    RSH("-rsh"),

    /**
     * Makes init call the specified function.
     * Func defaults to start.
     * If no arguments are provided, the function is assumed to be of arity 0.
     * Otherwise it is assumed to be of arity 1,
     * taking the list [Arg1,Arg2,...] as argument.
     * All arguments are passed as strings. See init(3).
     */
    RUN("-run"),

    /**
     * Makes init call the specified function.
     * Func defaults to start.
     * If no arguments are provided, the function is assumed to be of arity 0.
     * Otherwise it is assumed to be of arity 1,
     * taking the list [Arg1,Arg2,...] as argument.
     * All arguments are passed as atoms. See init(3).
     *
     * So it is the same as `-run`.
     */
    S("-s"),

    /**
     * Sets the magic cookie of the node to Cookie, see erlang:set_cookie/2.
     */
    SETCOOKIE("-setcookie"),

    /**
     * Specifies how long time (in milliseconds) the init process is allowed to
     * spend shutting down the system. If Time ms have elapsed, all processes
     * still existing are killed. Defaults to infinity.
     */
    SHUTDOWN_TIME("-shutdown_time"),

    /**
     * Makes the Erlang runtime system into a distributed node,
     * similar to -name, but the host name portion of the node
     * name Name@Host will be the short name, not fully qualified.
     *
     * This is sometimes the only way to run distributed Erlang
     * if the DNS (Domain Name System) is not running.
     * There can be no communication between nodes running with
     * the -sname flag and those running with the -name flag,
     * as node names must be unique in distributed Erlang systems.
     */
    SNAME("-sname"),

    /**
     * `-smp enable` and `-smp` starts the Erlang runtime system with SMP
     * support enabled. This may fail if no runtime system with SMP support
     * is available. `-smp auto` starts the Erlang runtime system with SMP
     * support enabled if it is available and more than one logical processor
     * are detected. `-smp disable` starts a runtime system without SMP support.
     *
     * NOTE: The runtime system with SMP support will not be available on all
     * supported platforms. See also the +S flag.
     */
    SMP("-smp");

    private final String flag;

    private InitFlag(String flag) {
        this.flag = flag;
    }

    @Override
    public String toString() {
        return flag;
    }

    public static boolean valid(String string) {
        return of(string) != null;
    }

    public static InitFlag of(String string) {
        for (InitFlag flag : values()) {
            if (flag.toString().equals(string)) {
                return flag;
            }
        }
        return null;
    }

}
