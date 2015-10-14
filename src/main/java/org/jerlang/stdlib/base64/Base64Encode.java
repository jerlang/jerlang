package org.jerlang.stdlib.base64;

import java.util.Base64;

import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Binary;
import org.jerlang.type.List;
import org.jerlang.type.Term;

public class Base64Encode {

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 1:
            return encode_1(params.head().toBinary());
        default:
            throw new Error("badarg");
        }
    }

    /**
     * Encodes a plain ASCII string into base64.
     * The result will be 33% larger than the data.
     */
    public static Term encode_1(Binary data) {
        return Binary.of(Base64.getEncoder().encode(data.toByteArray()));
    }

}
