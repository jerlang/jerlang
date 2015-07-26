package org.jerlang.stdlib.beam_lib;

import static org.jerlang.kernel.File.enoent;
import static org.jerlang.kernel.File.eperm;
import static org.jerlang.stdlib.BeamLib.beam_lib;
import static org.jerlang.stdlib.BeamLib.error;
import static org.jerlang.stdlib.BeamLib.file_error;
import static org.jerlang.stdlib.BeamLib.invalid_beam_file;
import static org.jerlang.stdlib.BeamLib.not_a_beam_file;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.EOFException;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

import org.jerlang.stdlib.Lists;
import org.jerlang.type.Atom;
import org.jerlang.type.Binary;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Str;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;

/**
 * Implementation of the beam_lib:md5/1 function.
 */
public class MD5 {

    /**
     * The following chunks are significant when calculating the MD5
     * for a module. They are listed in the order that they should be MD5:ed.
     */
    public static List md5_chunks() {
        return List.of(
            ChunkId.ATOM.toStr(),
            ChunkId.CODE.toStr(),
            ChunkId.STRT.toStr(),
            ChunkId.IMPT.toStr(),
            ChunkId.EXPT.toStr(),
            ChunkId.FUNT.toStr(),
            ChunkId.LITT.toStr()
            );
    }

    /**
     * Calculates an MD5 redundancy check for the code of the module
     * (compilation date and other attributes are not included).
     */
    public static Term md5(Str filename_term) {
        File file = new File(filename_term.string());
        Term result = new List();

        if (!file.exists()) {
            return Tuple.of(error, beam_lib, Tuple.of(file_error, filename_term, enoent));
        }

        if (!file.canRead()) {
            return Tuple.of(error, beam_lib, Tuple.of(file_error, filename_term, eperm));
        }

        try {
            byte[] bytes = Files.readAllBytes(file.toPath());
            result = do_md5(filename_term, bytes);
        } catch (FileNotFoundException fileNotFoundException) {
            return Tuple.of(error, beam_lib, Tuple.of(file_error, filename_term, enoent));
        } catch (EOFException eofException) {
            return Tuple.of(error, beam_lib, Tuple.of(invalid_beam_file, filename_term, Integer.of(0)));
        } catch (IOException ioException) {
            System.err.println("IOException: " + ioException);
        } catch (NoSuchAlgorithmException noSuchAlgorithmException) {
            System.err.println("NoSuchAlgorithmException: " + noSuchAlgorithmException);
        }

        return result;
    }

    private static Term do_md5(Term filename, byte[] bytes) throws IOException, NoSuchAlgorithmException {
        InputStream inputStream = new ByteArrayInputStream(bytes);
        Term chunks = get_chunks(filename, inputStream);
        MessageDigest md5 = init();
        if (chunks instanceof List) {
            List chunkList = (List) chunks;
            while (chunkList.head() != null) {
                Tuple chunk = (Tuple) chunkList.head();
                if (is_md5_chunk(chunk)) {
                    Integer offset = (Integer) chunk.element(1);
                    Integer size = (Integer) chunk.element(2);
                    update(md5, bytes, offset.toInt(), size.toInt());
                }
                chunkList = chunkList.tail();
            }
        }
        return Tuple.of(
            Atom.of("ok"),
            Tuple.of(
                Atom.of("pid"),
                new Binary(digest(md5))
                ));
    }

    private static boolean is_md5_chunk(Tuple chunk) {
        String chunk_name = chunk.element(0).toString();
        switch (chunk_name) {
        case "\"Atom\"":
        case "\"Code\"":
        case "\"StrT\"":
        case "\"ImpT\"":
        case "\"ExpT\"":
        case "\"FunT\"":
        case "\"LitT\"":
            return true;
        default:
            return false;
        }
    }

    private static Term get_chunks(Term filename, InputStream inputStream) throws IOException {
        List chunks = List.nil;
        DataInputStream dis = new DataInputStream(inputStream);
        if (dis.readInt() != 0x464f5231) { // "FOR1"
            return Tuple.of(not_a_beam_file, filename);
        }
        int length = dis.readInt();
        if (dis.readInt() != 0x4245414d) { // "BEAM"
            return Tuple.of(not_a_beam_file, filename);
        }
        int offset = 20;
        while (offset < length) {
            Chunk chunk = new Chunk(ChunkId.of(dis.readInt()), offset, dis.readInt());
            chunks = new List(chunk.asTuple(), chunks);
            offset += 8 + ((chunk.length() + 3) & ~3);
            dis.skipBytes((chunk.length() + 3) & ~3);
        }
        return Lists.reverse_1(chunks);
    }

    private static MessageDigest init() throws NoSuchAlgorithmException {
        return MessageDigest.getInstance("MD5");
    }

    private static void update(MessageDigest md5, byte[] bytes, int offset, int len) {
        md5.update(bytes, offset, len);
    }

    private static byte[] digest(MessageDigest md5) {
        return md5.digest();
    }

}
