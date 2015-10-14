package org.jerlang.stdlib.base64;

import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Binary;
import org.jerlang.type.List;
import org.jerlang.type.Term;

public class Base64Decode {

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 1:
            return decode_1(params.head().toBinary());
        default:
            throw new Error("badarg");
        }
    }

    /**
     * Decodes a base64 encoded string to plain ASCII.
     * See RFC4648.
     * decode/1 only strips away whitespace characters.
     */
    public static Term decode_1(Binary base64) {
        return List.nil;
    }

}
