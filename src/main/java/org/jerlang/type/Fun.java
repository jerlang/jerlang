package org.jerlang.type;

import java.lang.invoke.MethodHandle;

import org.jerlang.FunctionSignature;
import org.jerlang.Module;
import org.jerlang.erts.erlang.Error;
import org.jerlang.stdlib.beam_lib.LambdaInfo;

/**
 * http://erlang.org/doc/programming_examples/funs.html
 */
public class Fun extends Term {

    private final FunctionSignature signature;
    private final MethodHandle handle;

    private Module module;
    private LambdaInfo lambdaInfo;
    private Term[] saved_registers = {};

    public Fun(org.jerlang.Process proc, Module module, LambdaInfo lambdaInfo) {
        this(lambdaInfo.toFunctionSignature(module.name()), null);
        this.module = module;
        this.lambdaInfo = lambdaInfo;

        int numFree = lambdaInfo.numFree();
        if (numFree > 0) {
            saved_registers = new Term[numFree];
            System.arraycopy(proc.registers(), 0, saved_registers, 0, numFree);
        }
    }

    public Fun(FunctionSignature signature, MethodHandle handle) {
        this.signature = signature;
        this.handle = handle;
    }

    public Term apply(Term param) throws Error {
        try {
            return (Term) handle.invoke(param);
        } catch (Error e) {
            throw e;
        } catch (Throwable e) {
            e.printStackTrace();
            throw new Error("Could not apply " + param + " to " + signature);
        }
    }

    public MethodHandle handle() {
        return handle;
    }

    public Term[] savedRegisters() {
        return saved_registers;
    }

    public FunctionSignature signature() {
        return signature;
    }

    public Fun toFun() {
        return this;
    }

    @Override
    public String toString() {
        return "{Fun," + signature.toString() + "}";
    }

    public Term label() {
        return Tuple.of(Atom.of("label"), lambdaInfo.label());
    }

}
