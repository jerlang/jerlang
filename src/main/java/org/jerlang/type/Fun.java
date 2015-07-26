package org.jerlang.type;

import java.lang.invoke.MethodHandle;

import org.jerlang.FunctionSignature;

/**
 * http://erlang.org/doc/programming_examples/funs.html
 */
public class Fun extends Term {

    private final FunctionSignature signature;
    private final MethodHandle handle;

    public Fun(FunctionSignature signature, MethodHandle handle) {
        this.signature = signature;
        this.handle = handle;
    }

    public Term apply(Term param) {
        try {
            return (Term) handle.invoke(param);
        } catch (Throwable e) {
            e.printStackTrace();
            throw new Error("Could not apply " + param + " to " + signature);
        }
    }

    public MethodHandle handle() {
        return handle;
    }

    public FunctionSignature signature() {
        return signature;
    }

    public Fun toFun() {
        return this;
    }

}
