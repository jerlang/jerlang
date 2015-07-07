package org.jerlang.kernel.file;

import org.jerlang.type.Atom;

/**
 * = file
 * 
 * == MODULE
 * 
 * http://www.erlang.org/doc/man/file.html[file]
 * 
 * == DESCRIPTION
 * 
 * The module file provides an interface to the file system.
 *
 * On operating systems with thread support, it is possible to let file operations be performed in threads of their own, allowing other Erlang processes to continue executing in parallel with the file operations. See the command line flag +A in erl(1).
 *
 * With regard to file name encoding, the Erlang VM can operate in two modes. The current mode can be queried using the native_name_encoding/0 function. It returns either latin1 or utf8.
 *
 * In the latin1 mode, the Erlang VM does not change the encoding of file names. In the utf8 mode, file names can contain Unicode characters greater than 255 and the VM will convert file names back and forth to the native file name encoding (usually UTF-8, but UTF-16 on Windows).
 *
 * The default mode depends on the operating system. Windows and MacOS X enforce consistent file name encoding and therefore the VM uses the utf8 mode.
 *
 * On operating systems with transparent naming (i.e. all Unix systems except MacOS X), the default will be utf8 if the terminal supports UTF-8, otherwise latin1. The default may be overridden using the +fnl (to force latin1 mode) or +fnu (to force utf8 mode) when starting erl.
 *
 * On operating systems with transparent naming, files could be inconsistently named, i.e. some files are encoded in UTF-8 while others are encoded in (for example) iso-latin1. To be able to handle file systems with inconsistent naming when running in the utf8 mode, the concept of "raw file names" has been introduced.
 *
 * A raw file name is a file name given as a binary. The Erlang VM will perform no translation of a file name given as a binary on systems with transparent naming.
 *
 * When running in the utf8 mode, the file:list_dir/1 and file:read_link/1 functions will never return raw file names. Use the list_dir_all/1 and read_link_all/1 functions to return all file names including raw file names.
 *
 * Also see Notes about raw file names.
 *
 * == DATA TYPES
 *
 * ----
 * filename() = string()
 * ----
 *
 * ----
 * posix() = eacces
 *         | eagain
 *         | ebadf
 *         | ebusy
 *         | edquot
 *         | eexist
 *         | efault
 *         | efbig
 *         | eintr
 *         | einval
 *         | eio
 *         | eisdir
 *         | eloop
 *         | emfile
 *         | emlink
 *         | enametoolong
 *         | enfile
 *         | enodev
 *         | enoent
 *         | enomem
 *         | enospc
 *         | enotblk
 *         | enotdir
 *         | enotsup
 *         | enxio
 *         | eperm
 *         | epipe
 *         | erofs
 *         | espipe
 *         | esrch
 *         | estale
 *         | exdev
 * ----
 * An atom which is named from the POSIX error codes used in Unix,
 * and in the runtime libraries of most C compilers.
 */

public class File {

    /** No such file or directory (POSIX.1) */
    public static final Atom enoent = Atom.of("enoent");

    /** Operation not permitted (POSIX.1) */
    public static final Atom eperm = Atom.of("eperm");

    private File() {
    }

}
