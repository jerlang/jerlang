package org.jerlang.stdlib;

import org.jerlang.stdlib.base64.Base64Decode;
import org.jerlang.stdlib.base64.Base64DecodeToString;
import org.jerlang.stdlib.base64.Base64Encode;
import org.jerlang.stdlib.base64.Base64EncodeToString;
import org.jerlang.stdlib.base64.Base64MimeDecode;
import org.jerlang.stdlib.base64.Base64MimeDecodeToString;
import org.jerlang.type.Binary;
import org.jerlang.type.Term;

/**
 * = base64
 *
 * == Summary
 *
 * Implements base 64 encode and decode, see RFC2045.
 *
 * == Description
 *
 * Implements base 64 encode and decode, see RFC2045.
 *
 * Based on:
 * http://erlang.org/doc/man/base64.html
 */

public class Base64 {

    private Base64() {
    }

    public static Term decode(Binary base64) {
        return Base64Decode.decode_1(base64);
    }

    public static Term decode_to_string(Binary base64) {
        return Base64DecodeToString.decode_to_string_1(base64);
    }

    public static Term encode(Binary data) {
        return Base64Encode.encode_1(data);
    }

    public static Term encode_to_string(Binary data) {
        return Base64EncodeToString.encode_to_string_1(data);
    }

    public static Term mime_decode(Binary base64) {
        return Base64MimeDecode.mime_decode_1(base64);
    }

    public static Term mime_decode_to_string(Binary base64) {
        return Base64MimeDecodeToString.mime_decode_to_string_1(base64);
    }

}
