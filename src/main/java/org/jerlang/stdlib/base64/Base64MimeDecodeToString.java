package org.jerlang.stdlib.base64;

import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Binary;
import org.jerlang.type.List;
import org.jerlang.type.Term;

public class Base64MimeDecodeToString {

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 1:
            return mime_decode_to_string_1(params.head().toBinary());
        default:
            throw new Error("badarg");
        }
    }

    /**
     * Decodes a base64 encoded string to plain ASCII.
     * See RFC4648.
     * mime_decode_to_string/1 strips away illegal characters.
     */
    public static Term mime_decode_to_string_1(Binary base64) {
        return List.nil;
    }

}
