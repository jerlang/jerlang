package org.jerlang.stdlib.beam_lib;

import java.util.ArrayList;

import org.jerlang.type.List;

/**
 * = The Function Table Chunk
 *
 * The Function Table Chunk is an option chunk that stores the lambdas
 * (i.e. anonymous functions) of the module.
 *
 * The Function Table Chunk is composed of a header followed by one or
 * more function definitions.
 *
 * == Header
 *
 * The header is composed of 8 bytes.
 * This is the structure of the header:
 *
 * [cols="1,1,6", options="header"]
 * |===
 * |Length
 * |Value
 * |Description
 *
 * |4 bytes
 * |`FunT`
 * |The magic number indicating a Function Table Chunk.
 *
 * |4 bytes
 * |size
 * |Number of anonymous functions in this chunk
 * |===
 *
 * == Function record format
 *
 * Each anonymous function is defined by 6 32-bit values:
 *
 * [cols="1,1,6", options="header"]
 * |===
 * |Length
 * |Value
 * |Description
 *
 * | 4 bytes
 * | function atom id
 * | The index in Atom Chunk of the atom defining the function name
 *
 * | 4 bytes
 * | arity
 * | The number of arguments of the function
 *
 * | 4 bytes
 * | label
 * | The label in Code Chunk where function is defined
 *
 * | 4 bytes
 * | index
 * | TODO
 *
 * | 4 bytes
 * | num-free
 * | TODO
 *
 * | 4 bytes
 * | old-unique
 * | TODO
 * |===
 *
 * See also `beam_disasm_lambdas/2` in:
 * https://github.com/erlang/otp/blob/master/lib/compiler/src/beam_disasm.erl
*/
public class FunctionTableChunk extends Chunk {

    private ArrayList<LambdaInfo> lambdas;

    public FunctionTableChunk(int offset, int length) {
        super(ChunkId.FUNT, offset, length);
        lambdas = new ArrayList<>();
    }

    public void add(LambdaInfo lambdaInfo) {
        lambdas.add(lambdaInfo);
    }

    public List lambdas() {
        return List.of(lambdas);
    }

}
