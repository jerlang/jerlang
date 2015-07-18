package org.jerlang.stdlib.beam_lib;

import java.util.ArrayList;

import org.jerlang.FunctionSignature;
import org.jerlang.type.List;

/**
 * = Export Table Chunk
 *
 * The Export Table Chunk is used to define methods that this module exports,
 * and therefore those methods which can be called by other modules.
 *
 * == Header
 *
 * The Export Table Chunk Header is composed of 8 bytes.
 * This is the structure of the Export Table Chunk Header:
 *
 * [cols="2,3,11", options="header"]
 * |===
 * |Length
 * |Value
 * |Description
 *
 * |4 bytes
 * |`ExpT`
 * |Magic number indicating the Export Table Chunk.
 *
 * |4 bytes
 * |count
 * |Export Table Chunk length in the number of records
 * |===
 *
 * Note each record is 12 bytes.
 * The size of the data for the Export Table Chunk will be equal to
 * 12 bytes * number of records.
 *
 * == Export Record Format
 *
 * The Export Record Format is a fixed format of 12 bytes per record.
 * This is the structure of the Export Record Format:
 *
 * [cols="2,3,11", options="header"]
 * |===
 * |Length
 * |Value
 * |DescriptionLength Value Description
 *
 * |4 bytes
 * |method-atom-id
 * |An identifier of the atom used to identify the method name of the method
 *  to be exported. The identifier is used to look up the Atom in the Atom
 *  Chunk of this BEAM file. Note that for purposes of this index, the list
 *  of Atoms in the Atom Chunk is an index *starting at one*.
 *
 * |4 bytes
 * |arity
 * |The arity of the method to be exported
 *
 * |4 bytes
 * |module-label
 * |The code label identifing the starting position of the method within the
 *  Code Chunk of the BEAM file.
 * |===
 *
 * Together these 3 items contain the information required to being execution
 * at the appropriate point. We need to sets of information to begin execution
 * of a method. We need the method signature we are familiar with:
 * `math:cos/1` where
 *
 * . `math` is the module
 * . `cos` is the function name
 * `1` is the arity.
 *
 * ... and the location (label) in the code block to begin execution.
 * The module name is not required in the export table, since the module name
 * is known from the first entry in the Atom Chunk.
 * It is shared among all of the exported methods.
 * The other required information is contained within the Export Record.
 *
 * Source:
 * https://synrc.com/publications/cat/Functional%20Languages/Erlang/BEAM.pdf
 */
public class ExportTableChunk extends Chunk {

    private final ArrayList<FunctionSignature> exports;

    public ExportTableChunk(Chunk chunk) {
        super(ChunkId.EXPT, chunk);
        exports = new ArrayList<>();
    }

    public void add(FunctionSignature functionSignature) {
        exports.add(functionSignature);
    }

    public List exports() {
        return List.of(exports);
    }

}
