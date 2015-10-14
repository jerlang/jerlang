package org.jerlang.stdlib.base64;

import java.util.Base64;

import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Binary;
import org.jerlang.type.List;
import org.jerlang.type.Str;
import org.jerlang.type.Term;

public class Base64DecodeToString {

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 1:
            return decode_to_string_1(params.head().toBinary());
        default:
            throw new Error("badarg");
        }
    }

    /**
     * Decodes a base64 encoded string to plain ASCII.
     * See RFC4648.
     * decode_to_string/1 only strips away whitespace characters.
     */
    public static Term decode_to_string_1(Binary base64) {
        return Str.of(Base64.getDecoder().decode(base64.toByteArray()));
    }

}
