package org.jerlang.erts.emulator;

/**
 * Flags for bs_get_* / bs_put_* / bs_init* instructions.
 * 
 * Source:
 * https://github.com/erlang/otp/blob/maint/erts/emulator/beam/erl_bits.h
 */
public final class BitStringFlag {

    /** Field is guaranteed to be byte-aligned. */
    private static final int BSF_ALIGNED = 1;

    /** Field is little-endian (otherwise big-endian). */
    private static final int BSF_LITTLE = 2;

    /** Field is signed (otherwise unsigned). */
    private static final int BSF_SIGNED = 4;

    /** Size in bs_init is exact. */
    private static final int BSF_EXACT = 8;

    /** Native endian. */
    private static final int BSF_NATIVE = 16;

    public static final boolean isAligned(int flag) {
        return (flag & BSF_ALIGNED) == BSF_ALIGNED;
    }

    public static final boolean isExact(int flag) {
        return (flag & BSF_EXACT) == BSF_EXACT;
    }

    public static final boolean isLittle(int flag) {
        return (flag & BSF_LITTLE) == BSF_LITTLE;
    }

    public static final boolean isNative(int flag) {
        return (flag & BSF_NATIVE) == BSF_NATIVE;
    }

    public static final boolean isSigned(int flag) {
        return (flag & BSF_SIGNED) == BSF_SIGNED;
    }

    private BitStringFlag() {
        // You can not instantiate this class.
    }

}
