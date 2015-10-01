package org.jerlang.stdlib.beam_lib;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.EOFException;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.Files;

import org.jerlang.erts.erlang.Error;
import org.jerlang.stdlib.Lists;
import org.jerlang.type.Atom;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Str;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;
import org.jerlang.util.ByteUtil;

public class BeamLibInfo {

    private BeamLibInfo() {
    }

    public static final Atom beam_lib = Atom.of("beam_lib");
    public static final Atom enoent = Atom.of("enoent");
    public static final Atom eperm = Atom.of("eperm");
    public static final Atom error = Atom.of("error");
    public static final Atom file = Atom.of("file");
    public static final Atom file_error = Atom.of("file_error");
    public static final Atom invalid_beam_file = Atom.of("invalid_beam_file");
    public static final Atom not_a_beam_file = Atom.of("not_a_beam_file");

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 1:
            return info_1(params.head().toStr());
        default:
            throw new Error("badarg");
        }
    }

    public static Term info_1(Str filename_term) {
        File file = new File(filename_term.string());
        Term result = new List();

        if (!file.exists()) {
            return Tuple.of(error, beam_lib, Tuple.of(file_error, filename_term, enoent));
        }

        if (!file.canRead()) {
            return Tuple.of(error, beam_lib, Tuple.of(file_error, filename_term, eperm));
        }

        try {
            byte[] bytes = ByteUtil.maybe_decompress(Files.readAllBytes(file.toPath()));
            result = do_info(filename_term, bytes);
            if (result instanceof List) {
                result = new List(Tuple.of(Atom.of("file"), filename_term), (List) result);
            }
        } catch (FileNotFoundException fileNotFoundException) {
            return Tuple.of(error, beam_lib, Tuple.of(file_error, filename_term, enoent));
        } catch (EOFException eofException) {
            return Tuple.of(error, beam_lib, Tuple.of(invalid_beam_file, filename_term, Integer.ZERO));
        } catch (IOException ioException) {
            System.err.println("IOException: " + ioException);
        }

        return result;
    }

    private static Term do_info(Term filename, byte[] bytes) throws IOException {
        List chunks = List.nil;
        DataInputStream dis = new DataInputStream(new ByteArrayInputStream(bytes));

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
            // Unknown chunk tags are ignored
            if (chunk.id() != null && !chunk.id().skip()) {
                chunks = new List(chunk.asTuple(), chunks);
            }
            offset += 8 + ((chunk.length() + 3) & ~3);
            dis.skipBytes((chunk.length() + 3) & ~3);
        }
        return new List(Tuple.of(Atom.of("chunks"), Lists.reverse(chunks)));
    }

}
