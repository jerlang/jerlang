package org.jerlang.stdlib.base64;

import java.util.Base64;

import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Binary;
import org.jerlang.type.List;
import org.jerlang.type.Term;

public class Base64MimeDecode {

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 1:
            return mime_decode_1(params.head().toBinary());
        default:
            throw Error.badarg;
        }
    }

    /**
     * Decodes a base64 encoded string to plain ASCII.
     * See RFC4648.
     * mime_decode/1 strips away illegal characters.
     */
    public static Term mime_decode_1(Binary base64) {
        return Binary.of(Base64.getMimeDecoder().decode(base64.toByteArray()));
    }

}
