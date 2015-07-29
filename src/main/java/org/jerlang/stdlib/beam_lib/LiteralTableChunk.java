package org.jerlang.stdlib.beam_lib;

import java.util.ArrayList;

import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * = Literal Table Chunk
 *
 * The Literal Table Chunk is used to define literals used in the module.
 * These literals are stored in a compressed state in the BEAM file.
 *
 * == Header
 *
 * The Literal Table Chunk Header is composed of 8 bytes.
 * This is the structure of the Literal Table Chunk Header:
 *
 * [cols="1,1,6", options="header"]
 * |===
 * |Length
 * |Value
 * |Description
 *
 * |4 bytes
 * |`LitT`
 * |Magic number indicating the Literal Table Chunk.
 *
 * |4 bytes
 * |bytes
 * |Compressed Literal Table length in bytes
 * |===
 *
 * Note the bytes provided above is the bytes of the compressed record.
 *
 * == Compressed Literal Table Format
 *
 * The Literal Table Chunk contains a single Compressed Literal Table.
 * This Compressed Literal Table contains a 4-byte header followed by
 * the compressed data.
 * The header is a hint to the loader that indicates the uncompressed size of
 * the compressed data in the Compressed Literal Table.
 * This is the structure of the Compressed Literal Table Header:
 *
 * [cols="1,1,6", options="header"]
 * |===
 * |Length
 * |Value
 * |Description
 *
 * |4 bytes
 * |bytes
 * |Uncompressed Literal Table Length in bytes
 * |===
 *
 * The compression is done with zlib.
 * Once uncompressed, the result is the Uncompressed Literal Table.
 *
 * == Uncompressed Literal Table Format
 *
 * The Uncompressed Literal Table contains one or more literal records.
 * It contains a 4-byte header followed by the records.
 * The header indicates the number of literal records contained in the
 * Uncompressed Literal Table.
 * This is the structure of the Uncompressed Literal Table Header:
 *
 * [cols="1,1,6", options="header"]
 * |===
 * |Length
 * |Value
 * |Description
 *
 * |4 bytes
 * |records
 * |Number of Literal Records in the Uncompressed Literal Table
 * |===
 *
 * == Literal Record Format
 *
 * Each record in the Uncompressed Literal Table is composed of a header
 * and data. This is the structure of the Literal Record Format:
 *
 * [cols="1,1,6", options="header"]
 * |===
 * |Length
 * |Value
 * |Description
 *
 * |4 bytes
 * |n
 * |Number of bytes in the Literal
 *
 * |n
 * |bytes data
 * |Data used to store the Literal. This is in external format
 * |===
 *
 * Source:
 * https://synrc.com/publications/cat/Functional%20Languages/Erlang/BEAM.pdf
*/
public class LiteralTableChunk extends Chunk {

    private ArrayList<Term> literals;

    public LiteralTableChunk(Chunk chunk) {
        super(ChunkId.LITT, chunk);
        literals = new ArrayList<>();
    }

    public void add(Term literal) {
        literals.add(literal);
    }

    public List literals() {
        return List.of(literals);
    }

    @Override
    public String toString() {
        StringBuilder stringBuilder = new StringBuilder();
        stringBuilder.append("{literal_chunk,[");
        for (Term s : literals) {
            stringBuilder.append(s).append(',');
        }
        if (literals.size() > 0) {
            stringBuilder.setLength(stringBuilder.length() - 1);
        }
        stringBuilder.append("]}");
        return stringBuilder.toString();
    }

}
