package org.jerlang.stdlib.beam_lib;

import org.jerlang.FunctionSignature;
import org.jerlang.type.Atom;
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
        int functionId,
        int arity,
        int label,
        int index,
        int numFree,
        int oldUnique,
        int oldIndex,
        AtomChunk atomChunk) {
        super(2);
        set(1, Integer.of(oldIndex));
        set(2, Tuple.of(
            atomChunk.atoms()[functionId - 1],
            Integer.of(arity),
            Integer.of(label),
            Integer.of(index),
            Integer.of(numFree),
            Integer.of(oldUnique)));
    }

    public FunctionSignature toFunctionSignature(Atom module) {
        Tuple tuple = element(2).toTuple();
        Atom function = tuple.element(1).toAtom();
        Integer arity = tuple.element(2).toInteger();
        return new FunctionSignature(module, function, arity);
    }

    public Integer label() {
        Tuple tuple = element(2).toTuple();
        return tuple.element(3).toInteger();
    }

    public int numFree() {
        Tuple tuple = element(2).toTuple();
        return tuple.element(5).toInteger().toInt();
    }

}
