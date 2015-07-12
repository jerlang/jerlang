package org.jerlang.stdlib.beam_lib;

/**
 * = The Atom Chunk
 *
 * The Atom Chunk stores Atoms - the constant values (including identifiers
 * like the module name and function names). The Atom Chunk is composed of
 * a Header followed by Atom definitions.
 *
 * == Header
 *
 * The Atom Chunk Header is composed of 12 bytes. This is the structure of the
 * Atom Chunk Header:
 *
 * |===
 * |Length |Value |Description
 * |4 bytes|"Atom"|Magic number indicating the Atom Chunk.
 * |4 bytes|size  |Atom Chunk length in bytes
 * |4 bytes|count |Number of atoms in the Atom Chunk
 * |===
 *
 * == Atom Definition Format
 *
 * Each Atom Definition is composed of two items:
 * This is the structure of the Atom Definition:
 *
 * |===
 * |Length|Value  |Description
 * |1 bytes|length|Number of bytes in the Atom
 * |n bytes|atom  |The Atom (in the form of ASCII characters).
 * |===
 *
 * === Restriction
 * 
 * * Atoms are limited to 255 characters or less.
 * * Characters must be valid characters in ISO 8859-1.
 *
 * == Requirements
 *
 * The first Atom to appear in the Atom Chunk must be the module name.
 *
 * Source:
 * https://synrc.com/publications/cat/Functional%20Languages/Erlang/BEAM.pdf
 */
public class AtomChunk extends Chunk {

    public AtomChunk(int offset, int length) {
        super(ChunkId.ATOM, offset, length);
    }

}
