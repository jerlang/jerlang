package org.jerlang.stdlib.beam_lib;

import java.util.ArrayList;

import org.jerlang.FunctionSignature;
import org.jerlang.type.List;

/**
 * = Import Table Chunk
 *
 * The Import Table Chunk is used to define methods in other modules that are
 * called from the current module.
 *
 * == Header
 *
 * The Import Table Chunk Header is composed of 8 bytes.
 * This is the structure of the Import Table Chunk Header:
 *
 * [cols="2,3,11", options="header"]
 * |===
 * |Length
 * |Value
 * |Description
 *
 * |4 bytes
 * |`ImpT`
 * |Magic number indicating the Import Table Chunk.
 *
 * |4 bytes
 * |count
 * |Import Table Chunk length in the number of records
 * |===
 *
 * Note each record is 12 bytes.
 * The size of the data for the Import Table Chunk will be equal to
 * 12 bytes * number of records.
 *
 * == Import Record Format
 *
 * The Import Record Format is a fixed format of 12 bytes per record.
 * This is the structure of the Import Record Format:
 *
 * [cols="2,3,11", options="header"]
 * |===
 * |Length
 * |Value
 * |Description
 *
 * |4 bytes
 * |module-atom-id
 * |An identifier of the atom used to identify the module from which the method
 *  is imported. The identifier is used to look up the Atom in the Atom Chunk
 *  of this BEAM file.
 *
 * |4 bytes
 * |method-atom-id
 * |An identifier of the atom used to identify the method name of the method to
 *  be imported. The identifier is used to look up the Atom in the Atom Chunk
 *  of this BEAM file.
 *
 * |4 bytes
 * |arity
 * |The arity of the method to be imported
 * |===
 *
 * Together these 3 items contain the information we would normally associate
 * with a method signature, the module, function name, and arity, such as:
 * `math:cos/1` where
 *
 * . `math` is the module
 * . `cos` is the function name
 * . `1` is the arity.
 *
 * Source:
 * https://synrc.com/publications/cat/Functional%20Languages/Erlang/BEAM.pdf
 */
public class ImportTableChunk extends Chunk {

    private final ArrayList<FunctionSignature> imports;

    public ImportTableChunk(Chunk chunk) {
        super(ChunkId.IMPT, chunk);
        imports = new ArrayList<>();
    }

    public void add(FunctionSignature functionSignature) {
        imports.add(functionSignature);
    }

    public List imports() {
        return List.of(imports);
    }

}
