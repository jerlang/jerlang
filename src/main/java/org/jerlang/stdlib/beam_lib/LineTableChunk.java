package org.jerlang.stdlib.beam_lib;

import org.jerlang.type.Str;

public class LineTableChunk extends Chunk {

    private int version;
    private int flags;
    private int count;
    private Str[] fnames;
    private LineRecord[] records;

    public LineTableChunk(
        Chunk chunk,
        int version,
        int flags,
        int count,
        int records,
        int fnames) {
        super(ChunkId.LINE, chunk);
        this.version = version;
        this.flags = flags;
        this.count = count;
        this.records = new LineRecord[records];
        this.fnames = new Str[fnames];
    }

    public int count() {
        return count;
    }

    public int flags() {
        return flags;
    }

    public Str[] fnames() {
        return fnames;
    }

    public LineRecord[] records() {
        return records;
    }

    public int version() {
        return version;
    }

    @Override
    public String toString() {
        StringBuilder stringBuilder = new StringBuilder();
        stringBuilder.append("{lines,[");
        stringBuilder.append("{records,[");
        for (LineRecord lineRecord : records) {
            stringBuilder.append(lineRecord).append(',');
        }
        stringBuilder.append("]]}");
        stringBuilder.append("{filenames,[");
        for (Str filename : fnames) {
            stringBuilder.append(filename).append(',');
        }
        stringBuilder.append("]]}");
        return stringBuilder.toString();
    }

}
