package org.jerlang.stdlib.beam_lib;

import java.io.DataInputStream;

import org.jerlang.type.Atom;
import org.jerlang.type.Integer;
import org.jerlang.type.Str;
import org.jerlang.type.Term;

public class LineTableChunkReader extends AbstractChunkReader<LineTableChunk> {

    private final AtomChunk atomChunk;

    public LineTableChunkReader(Chunk chunk, DataInputStream inputStream, AtomChunk atomChunk) {
        super(chunk, inputStream);
        this.atomChunk = atomChunk;
    }

    public LineTableChunk read() throws Throwable {
        LineTableChunk lineTableChunk = new LineTableChunk(chunk(),
            read4Bytes(), read4Bytes(), read4Bytes(), read4Bytes(), read4Bytes());

        // From beam_asm.erl:
        // "Encode the line items compactly.
        // Tag the FnameIndex with an 'a' tag (atom) and
        // only include it when it has changed.
        // Tag the line numbers with an 'i' (integer) tag."
        Atom filename = atomChunk.atoms()[0];
        for (int index = 0; index < lineTableChunk.records().length; index++) {
            Term t = decodeArg(null, null);
            if (t instanceof Atom) {
                filename = t.toAtom();
            } else if (t instanceof Integer) {
                lineTableChunk.records()[index] = new LineRecord(filename, t.toInteger().toInt());
            }
        }

        for (int index = 0; index < lineTableChunk.fnames().length; index++) {
            int len = read2Bytes();
            byte[] bytes = new byte[len];
            readBytes(bytes);
            lineTableChunk.fnames()[index] = new Str(new String(bytes));
        }

        return lineTableChunk;
    }

}
