package org.jerlang.type;

import org.jerlang.FunctionSignature;
import org.jerlang.Process;
import org.jerlang.type.stack.BinMatchState;
import org.jerlang.type.stack.ExceptionHandler;

/**
 * A piece of data of any data type is called a term.
 *
 * http://www.erlang.org/doc/reference_manual/data_types.html
 */
public class Term {

    public static Term of(String string) {
        if (string == null) {
            return List.nil;
        }

        if (string.isEmpty()) {
            return List.nil;
        }

        return new Str(string);
    }

    public boolean isFalse() {
        throw new Error("Cannot convert to boolean: " + this);
    }

    public boolean isList() {
        return false;
    }

    public boolean isTrue() {
        throw new Error("Cannot convert to boolean: " + this);
    }

    public boolean isFRegister() {
        return false;
    }

    public boolean isXRegister() {
        return false;
    }

    public boolean isYRegister() {
        return false;
    }

    public Term toArg(Process p) {
        if (isXRegister()) {
            return p.getX(toRegisterIndex());
        } else if (isYRegister()) {
            return p.getY(toRegisterIndex());
        } else {
            return this;
        }
    }

    public Atom toAtom() {
        throw new Error("Cannot convert to atom: " + this);
    }

    public Binary toBinary() {
        throw new Error("Cannot convert to binary: " + this);
    }

    public BitString toBitString() {
        throw new Error("Cannot convert to bitstring: " + this);
    }

    public BinMatchState toBinMatchState() {
        throw new Error("Cannot convert to bin_match_state: " + this);
    }

    public ExceptionHandler toExceptionHandler() {
        throw new Error("Cannot convert to exception handler: " + this);
    }

    public Float toFloat() {
        throw new Error("Cannot convert to float: " + this);
    }

    public Fun toFun() {
        throw new Error("Cannot convert to fun: " + this);
    }

    public FunctionSignature toFunctionSignature() {
        throw new Error("Cannot convert to function signature: " + this);
    }

    public Integer toInteger() {
        throw new Error("Cannot convert to integer: " + this);
    }

    public List toList() {
        throw new Error("Cannot convert to list: " + this);
    }

    public Map toMap() {
        throw new Error("Cannot convert to map: " + this);
    }

    public PID toPID() {
        throw new Error("Cannot convert to pid: " + this);
    }

    public Reference toReference() {
        throw new Error("Cannot convert to reference: " + this);
    }

    public Integer toRegisterIndex() {
        throw new Error("Cannot convert to register index: " + this);
    }

    public Str toStr() {
        throw new Error("Cannot convert to str: " + this);
    }

    public TimerReference toTimerReference() {
        throw new Error("Cannot convert to timer reference: " + this);
    }

    public Tuple toTuple() {
        throw new Error("Cannot convert to tuple: " + this);
    }

}
