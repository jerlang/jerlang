package org.jerlang.util;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;

import com.jcraft.jzlib.GZIPInputStream;

public class ByteUtil {

    public static byte[] maybe_decompress(byte[] bytes) {
        if (bytes.length == 0) {
            return bytes;
        }
        if (bytes[0] != 0x1f) {
            return bytes;
        }
        try {
            InputStream in = new GZIPInputStream(new ByteArrayInputStream(bytes));
            ByteArrayOutputStream out = new ByteArrayOutputStream();
            int symbol;
            while ((symbol = in.read()) != -1) {
                out.write(symbol);
            }
            in.close();
            bytes = out.toByteArray();
        } catch (IOException e) {
            e.printStackTrace();
        }
        return bytes;
    }

}
