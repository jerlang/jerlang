package org.jerlang.stdlib.beam_lib;

import org.jerlang.type.Binary;
import org.jerlang.type.Integer;

/**
 * = The String Table Chunk
 *
 * The String Table ("StrT") Chunk is a mandatory chunk that stores the
 * strings used for pattern matching.
 *
 * The chunk itself is just a single binary with a length.
 *
 * It is defined in `lib/compiler/src/beam_dict.erl` as an `asm` record,
 * where only the `strings` and `string_offset` fields are used.
 *
 * In code chunk, binaries and bitstrings may be referenced by an offset and
 * length to the single binary in the string table.
 */
public class StringTableChunk extends Chunk {

    private final Binary strings;
    private final Integer size;

    public StringTableChunk(int offset, int length, byte[] bytes) {
        super(ChunkId.STRT, offset, length);
        size = Integer.of(length);
        strings = Binary.of(bytes);
    }

    public Binary strings() {
        return strings;
    }

    public Integer size() {
        return size;
    }

}
