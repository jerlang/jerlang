package org.jerlang.erts;

/**
 * See:
 * http://erlang.org/doc/apps/erts/erl_ext_dist.html
 */
public enum ExternalTermFormatTag {

    /**
     * Refers to the atom with `AtomCacheReferenceIndex`
     * in the distribution header.
     *
     * http://erlang.org/doc/apps/erts/erl_ext_dist.html#id92929
     */
    ATOM_CACHE_REF(82),

    /**
     * Unsigned 8 bit integer.
     *
     * http://erlang.org/doc/apps/erts/erl_ext_dist.html#id93004
     */
    SMALL_INTEGER_EXT(97),

    /**
     * Signed 32 bit integer in big-endian format (i.e. MSB first)
     *
     * http://erlang.org/doc/apps/erts/erl_ext_dist.html#id93068
     */
    INTEGER_EXT(98),

    /**
     * A float is stored in string format.
     * The format used in sprintf to format the float is "%.20e"
     * (there are more bytes allocated than necessary).
     * To unpack the float use sscanf with format "%lf".
     *
     * This term is used in minor version 0 of the external format;
     * it has been superseded by NEW_FLOAT_EXT.
     *
     * http://erlang.org/doc/apps/erts/erl_ext_dist.html#id93132
     */
    FLOAT_EXT(99),

    /**
     * An atom is stored with a 2 byte unsigned length in big-endian order,
     * followed by Len numbers of 8 bit Latin1 characters that forms the
     * AtomName.
     *
     * NOTE: The maximum allowed value for Len is 255.
     *
     * http://erlang.org/doc/apps/erts/erl_ext_dist.html#id93208
     */
    ATOM_EXT(100),

    /**
     * Encode a reference object (an object generated with `make_ref/0`).
     * The Node term is an encoded atom, i.e.
     * `ATOM_EXT`, `SMALL_ATOM_EXT` or `ATOM_CACHE_REF`.
     * The ID field contains a big-endian unsigned integer,
     * but should be regarded as uninterpreted data since this field is
     * node specific. Creation is a byte containing a node serial number
     * that makes it possible to separate old (crashed) nodes from a new one.
     *
     * In ID, only 18 bits are significant; the rest should be 0. In Creation,
     * only 2 bits are significant; the rest should be 0.
     * See `NEW_REFERENCE_EXT`.
     *
     * http://erlang.org/doc/apps/erts/erl_ext_dist.html#id93307
     */
    REFERENCE_EXT(101),

    /**
     * Encode a port object (obtained form `open_port/2`).
     * The ID is a node specific identifier for a local port.
     * Port operations are not allowed across node boundaries.
     * The Creation works just like in `REFERENCE_EXT`.
     *
     * http://erlang.org/doc/apps/erts/erl_ext_dist.html#id93464
     */
    PORT_EXT(102),

    /**
     * Encode a process identifier object (obtained from spawn/3 or friends).
     * The ID and Creation fields works just like in `REFERENCE_EXT`,
     * while the Serial field is used to improve safety.
     * In ID, only 15 bits are significant; the rest should be 0.
     *
     * http://erlang.org/doc/apps/erts/erl_ext_dist.html#id93583
     */
    PID_EXT(103),

    /**
     * SMALL_TUPLE_EXT encodes a tuple.
     * The Arity field is an unsigned byte that determines how many elements
     * follow in the Elements section.
     *
     * http://erlang.org/doc/apps/erts/erl_ext_dist.html#id93727
     */
    SMALL_TUPLE_EXT(104),

    /**
     * Same as SMALL_TUPLE_EXT with the exception that Arity is an
     * unsigned 4 byte integer in big endian format.
     *
     * http://erlang.org/doc/apps/erts/erl_ext_dist.html#id93819
     */
    LARGE_TUPLE_EXT(105),

    /**
     * `MAP_EXT` encodes a map.
     * The Arity field is an unsigned 4 byte integer in big endian format
     * that determines the number of key-value pairs in the map.
     *
     * Key and value pairs (`Ki => Vi`) are encoded in the Pairs section in
     * the following order:
     *
     * `K1, V1, K2, V2,..., Kn, Vn`
     *
     * Duplicate keys are not allowed within the same map.
     *
     * http://erlang.org/doc/apps/erts/erl_ext_dist.html#id93909
     */
    MAP_EXT(116),

    /**
     * The representation for an empty list, i.e. the Erlang syntax `[]`.
     *
     * http://erlang.org/doc/apps/erts/erl_ext_dist.html#id94020
     */
    NIL_EXT(106),

    /**
     * String does NOT have a corresponding Erlang representation,
     * but is an optimization for sending lists of bytes
     * (integer in the range 0-255) more efficiently over the distribution.
     *
     * Since the Length field is an unsigned 2 byte integer (big endian),
     * implementations must make sure that lists longer than 65535 elements
     * are encoded as `LIST_EXT`.
     *
     * http://erlang.org/doc/apps/erts/erl_ext_dist.html#id94071
     */
    STRING_EXT(107),

    /**
     * Length is the number of elements that follows in the Elements section.
     * Tail is the final tail of the list; it is `NIL_EXT` for a proper list,
     * but may be anything type if the list is improper (for instance `[a|b]`).
     *
     * http://erlang.org/doc/apps/erts/erl_ext_dist.html#id94163
     */
    LIST_EXT(108),

    /**
     * Binaries are generated with bit syntax expression or with
     * `list_to_binary/1`, `term_to_binary/1`, or as input from binary ports.
     * The Len length field is an unsigned 4 byte integer (big endian).
     *
     * http://erlang.org/doc/apps/erts/erl_ext_dist.html#id94282
     */
    BINARY_EXT(109),

    /**
     * Bignums are stored in unary form with a Sign byte that is 0 if the binum
     * is positive and 1 if is negative.
     *
     * The digits are stored with the LSB byte stored first.
     * To calculate the integer the following formula can be used:
     *
     * ---
     * B = 256
     * (d0*B^0 + d1*B^1 + d2*B^2 + ... d(N-1)*B^(n-1))
     * ---
     *
     * http://erlang.org/doc/apps/erts/erl_ext_dist.html#id94379
     */
    SMALL_BIG_EXT(110),

    /**
     * Same as `SMALL_BIG_EXT` with the difference that the length field
     * is an unsigned 4 byte integer.
     *
     * http://erlang.org/doc/apps/erts/erl_ext_dist.html#id94486
     */
    LARGE_BIG_EXT(111),

    /**
     * Node and Creation are as in REFERENCE_EXT.
     *
     * ID contains a sequence of big-endian unsigned integers
     * (4 bytes each, so N' is a multiple of 4),
     * but should be regarded as uninterpreted data.
     *
     * ---
     * N' = 4 * Len.
     * ---
     *
     * In the first word (four bytes) of ID, only 18 bits are significant,
     * the rest should be 0. In Creation, only 2 bits are significant,
     * the rest should be 0.
     *
     * `NEW_REFERENCE_EXT` was introduced with distribution version 4.
     * In version 4, N' should be at most 12. See `REFERENCE_EXT`.
     *
     * http://erlang.org/doc/apps/erts/erl_ext_dist.html#id94589
     */
    NEW_REFERENCE_EXT(114),

    /**
     * An atom is stored with a 1 byte unsigned length, followed by Len numbers
     * of 8 bit Latin1 characters that forms the AtomName.
     * Longer atoms can be represented by `ATOM_EXT`.
     * Note the `SMALL_ATOM_EXT` was introduced in erts version 5.7.2 and require
     * an exchange of the `DFLAG_SMALL_ATOM_TAGS` distribution flag in the
     * distribution handshake.
     *
     * http://erlang.org/doc/apps/erts/erl_ext_dist.html#id94762
     */
    SMALL_ATOM_EXT(115),

    /**
     * Pid::
     * is a process identifier as in PID_EXT.
     * It represents the process in which the fun was created.
     *
     * Module::
     * is an encoded as an atom, using ATOM_EXT, SMALL_ATOM_EXT or
     * ATOM_CACHE_REF. This is the module that the fun is implemented in.
     *
     * Index::
     * is an integer encoded using SMALL_INTEGER_EXT or INTEGER_EXT.
     * It is typically a small index into the module's fun table.
     *
     * Uniq::
     * is an integer encoded using SMALL_INTEGER_EXT or INTEGER_EXT.
     * Uniq is the hash value of the parse for the fun.
     *
     * Free vars::
     * is NumFree number of terms, each one encoded according to its type.
     *
     * http://erlang.org/doc/apps/erts/erl_ext_dist.html#id94880
     */
    FUN_EXT(117),

    /**
     * This is the new encoding of internal funs:
     * `fun F/A and fun(Arg1,..) -> ... end`.
     *
     * Size::
     * is the total number of bytes, including the Size field.
     *
     * Arity::
     * is the arity of the function implementing the fun.
     *
     * Uniq::
     * is the 16 bytes MD5 of the significant parts of the Beam file.
     *
     * Index::
     * is an index number. Each fun within a module has an unique index.
     * Index is stored in big-endian byte order.
     *
     * NumFree::
     * is the number of free variables.
     *
     * Module::
     * is an encoded as an atom, using ATOM_EXT, SMALL_ATOM_EXT or ATOM_CACHE_REF.
     * This is the module that the fun is implemented in.
     *
     * OldIndex::
     * is an integer encoded using SMALL_INTEGER_EXT or INTEGER_EXT.
     * It is typically a small index into the module's fun table.
     *
     * OldUniq::
     * is an integer encoded using SMALL_INTEGER_EXT or INTEGER_EXT.
     * Uniq is the hash value of the parse tree for the fun.
     *
     * Pid::
     * is a process identifier as in PID_EXT.
     * It represents the process in which the fun was created.
     *
     * Free vars::
     * is NumFree number of terms, each one encoded according to its type.
     *
     * http://erlang.org/doc/apps/erts/erl_ext_dist.html#id95127
     */
    NEW_FUN_EXT(112),

    /**
     * This term is the encoding for external funs: `fun M:F/A`.
     *
     * `Module` and `Function` are atoms
     * (encoded using `ATOM_EXT`, `SMALL_ATOM_EXT` or `ATOM_CACHE_REF`).
     *
     * `Arity` is an integer encoded using `SMALL_INTEGER_EXT`.
     *
     * http://erlang.org/doc/apps/erts/erl_ext_dist.html#id95505
     */
    EXPORT_EXT(113),

    /**
     * This term represents a bitstring whose length in bits does not have to
     * be a multiple of 8.
     * The Len field is an unsigned 4 byte integer (big endian).
     * The Bits field is the number of bits (1-8) that are used in the last
     * byte in the data field, counting from the most significant bit towards
     * the least significant.
     *
     * http://erlang.org/doc/apps/erts/erl_ext_dist.html#id95648
     */
    BIT_BINARY_EXT(77),

    /**
     * A float is stored as 8 bytes in big-endian IEEE format.
     *
     * This term is used in minor version 1 of the external format.
     *
     * http://erlang.org/doc/apps/erts/erl_ext_dist.html#id95755
     */
    NEW_FLOAT_EXT(70),

    /**
     * An atom is stored with a 2 byte unsigned length in big-endian order,
     * followed by Len bytes containing the AtomName encoded in UTF-8.
     *
     * For more information on encoding of atoms,
     * see note on UTF-8 encoded atoms in the beginning of this document.
     *
     * http://erlang.org/doc/apps/erts/erl_ext_dist.html#id95823
     */
    ATOM_UTF8_EXT(118),

    /**
     * An atom is stored with a 1 byte unsigned length,
     * followed by Len bytes containing the AtomName encoded in UTF-8.
     * Longer atoms encoded in UTF-8 can be represented using ATOM_UTF8_EXT.
     *
     * For more information on encoding of atoms,
     * see note on UTF-8 encoded atoms in the beginning of this document.
     *
     * http://erlang.org/doc/apps/erts/erl_ext_dist.html#id95924
     */
    SMALL_ATOM_UTF8_EXT(119);

    private final int tag;

    private ExternalTermFormatTag(int tag) {
        this.tag = tag;
    }

    public int tag() {
        return tag;
    }

    public static ExternalTermFormatTag of(int value) {
        for (ExternalTermFormatTag tag : values()) {
            if (tag.tag == value) {
                return tag;
            }
        }
        return null;
    }

}
