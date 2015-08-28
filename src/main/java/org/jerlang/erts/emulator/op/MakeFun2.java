package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.Fun;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Creates a new Fun, based on the LambdaInfo on index Index in
 * the BEAM file function table chunk, and stores it in X0.
 *
 * The new Fun object saves also NumFree X registers,
 * which are copied to the registers when 'call_fun' is invoked.
 *
 * As an example:
 *
 * 1. X0 contains 2.
 * 2. make_fun2 is called with a lambda of arity 1 and num_free of 1.
 * 3. Fun saves X0 (because num_free is 1) to internal array.
 * 4. call_fun is invoked (where X0 to X(arity-1) are args and
 *    X(arity) contains Fun.
 * 5. Before the lambda is executed, we need to copy the num_free
 *    registers saved in step 3 and append to X registers,
 *    starting at X(arity).
 *
 * Args:
 * 1. Index
 *
 * Example:
 * {make_fun2,0}
 */
public class MakeFun2 {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        int index = i.arg(0).toInteger().toInt();
        Fun fun = new Fun(proc, m, m.beamData().functionTableChunk().lambdas().get(index));
        proc.setX(0, fun);
        return null;
    }

}
