package org.jerlang.erts.emulator;

/**
 * See:
 * http://www.erlang.org/doc/man/erl.html
 */
public enum Environment {

    /**
     * If the emulator needs to write a crash dump, the value of this variable
     * will be the file name of the crash dump file.
     * If the variable is not set, the name of the crash dump file will be
     * `erl_crash.dump` in the current directory.
     */
    ERL_CRASH_DUMP,

    /**
     * *Unix systems*:
     * If the emulator needs to write a crash dump, it will use the value of
     * this variable to set the nice value for the process, thus lowering its
     * priority. The allowable range is 1 through 39 (higher values will be
     * replaced with 39). The highest value, 39, will give the process the
     * lowest priority.
     */
    ERL_CRASH_DUMP_NICE,

    /**
     * *Unix systems*:
     * This variable gives the number of seconds that the emulator will be
     * allowed to spend writing a crash dump. When the given number of seconds
     * have elapsed, the emulator will be terminated by a SIGALRM signal.
     * 
     * If the environment variable is not set or it is set to zero seconds,
     * `ERL_CRASH_DUMP_SECONDS=0`, the runtime system will not even attempt to
     * write the crash dump file. It will just terminate.
     * 
     * If the environment variable is set to negative valie, e.g.
     * `ERL_CRASH_DUMP_SECONDS=-1`, the runtime system will wait indefinitely
     * for the crash dump file to be written.
     * 
     * This environment variable is used in conjuction with heart if heart
     * is running:
     * 
     * `ERL_CRASH_DUMP_SECONDS=0`::
     * Suppresses the writing a crash dump file entirely, thus rebooting the
     * runtime system immediately. This is the same as not setting the
     * environment variable.
     * 
     * `ERL_CRASH_DUMP_SECONDS=-1`::
     * Setting the environment variable to a negative value will cause the
     * termination of the runtime system to wait until the crash dump file
     * has been completly written.
     * 
     * `ERL_CRASH_DUMP_SECONDS=S`::
     * Will wait for S seconds to complete the crash dump file and then
     * terminate the runtime system.
     */
    ERL_CRASH_DUMP_SECONDS,

    /**
     * The content of this environment variable will be added to the beginning
     * of the command line for erl.
     * 
     * The -extra flag is treated specially.
     * Its scope ends at the end of the environment variable content.
     * Arguments following an -extra flag are moved on the command line into
     * the -extra section, i.e. the end of the command line following after an
     * -extra flag.
     */
    ERL_AFLAGS,

    /**
     * The content of these environment variables will be added to the end of
     * the command line for erl.
     * 
     * The -extra flag is treated specially.
     * Its scope ends at the end of the environment variable content.
     * Arguments following an -extra flag are moved on the command line into
     * the -extra section, i.e. the end of the command line following after an
     * -extra flag.
     */
    ERL_ZFLAGS,
    ERL_FLAGS,

    /**
     * This environment variable contains a list of additional library
     * directories that the code server will search for applications and
     * add to the code path. See code(3).
     */
    ERL_LIBS,

    /**
     * This environment variable may be set to a comma-separated list of
     * IP addresses, in which case the epmd daemon will listen only on the
     * specified address(es) and on the loopback address (which is implicitly
     * added to the list if it has not been specified).
     */
    ERL_EPMD_ADDRESS,

    /**
     * This environment variable can contain the port number to use when
     * communicating with epmd. The default port will work fine in most cases.
     * A different port can be specified to allow nodes of independent
     * clusters to co-exist on the same host. All nodes in a cluster must use
     * the same epmd port number.
     */
    ERL_EPMD_PORT;

    public String get() {
        return System.getenv(name());
    }

}
