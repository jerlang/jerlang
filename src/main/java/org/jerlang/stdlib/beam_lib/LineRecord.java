package org.jerlang.stdlib.beam_lib;

import org.jerlang.type.Atom;

public class LineRecord {

    private final Atom filename;
    private final int lineNumber;

    public LineRecord(Atom filename, int lineNumber) {
        this.filename = filename;
        this.lineNumber = lineNumber;
    }

    public Atom filename() {
        return filename;
    }

    public int lineNumber() {
        return lineNumber;
    }

    @Override
    public String toString() {
        return new StringBuilder()
            .append("{line_record,")
            .append(filename)
            .append(',')
            .append(lineNumber)
            .append('}')
            .toString();
    }

}
