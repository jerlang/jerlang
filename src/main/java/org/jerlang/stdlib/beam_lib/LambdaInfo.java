package org.jerlang.stdlib.beam_lib;

import org.jerlang.type.Integer;
import org.jerlang.type.Tuple;

/**
 * Java representation of type `l_info` defined in
 * https://github.com/erlang/otp/blob/master/lib/compiler/src/beam_disasm.erl
 * as
 * `{non_neg_integer(), {_,_,_,_,_,_}}`
 */
public class LambdaInfo extends Tuple {

    public LambdaInfo(
        int oldIndex,
        int functionId,
        int arity,
        int label,
        int index,
        int numFree,
        int oldUnique,
        AtomChunk atomChunk) {
        super(2);
        set(0, Integer.of(oldIndex));
        set(1, Tuple.of(
            atomChunk.atoms()[functionId - 1],
            Integer.of(arity),
            Integer.of(label),
            Integer.of(index),
            Integer.of(numFree),
            Integer.of(oldUnique)
            ));
    }

}
