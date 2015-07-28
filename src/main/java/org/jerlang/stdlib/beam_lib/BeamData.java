package org.jerlang.stdlib.beam_lib;

/**
 * This class represents a BEAM file.
 */
public class BeamData {

    private final AtomChunk atomChunk;
    private final AttributeChunk attributeChunk;
    private final CodeChunk codeChunk;
    private final ExportTableChunk exportTableChunk;
    private final FunctionTableChunk functionTableChunk;
    private final ImportTableChunk importTableChunk;
    private final LiteralTableChunk literalTableChunk;
    private final LocalFunctionTableChunk localFunctionTableChunk;
    private final StringTableChunk stringTableChunk;

    public BeamData(
        AtomChunk atomChunk,
        AttributeChunk attributeChunk,
        CodeChunk codeChunk,
        ExportTableChunk exportTableChunk,
        FunctionTableChunk functionTableChunk,
        ImportTableChunk importTableChunk,
        LiteralTableChunk literalTableChunk,
        LocalFunctionTableChunk localFunctionTableChunk,
        StringTableChunk stringTableChunk) {
        this.atomChunk = atomChunk;
        this.attributeChunk = attributeChunk;
        this.codeChunk = codeChunk;
        this.exportTableChunk = exportTableChunk;
        this.functionTableChunk = functionTableChunk;
        this.importTableChunk = importTableChunk;
        this.literalTableChunk = literalTableChunk;
        this.localFunctionTableChunk = localFunctionTableChunk;
        this.stringTableChunk = stringTableChunk;
    }

    public AtomChunk atomChunk() {
        return atomChunk;
    }

    public AttributeChunk attributeChunk() {
        return attributeChunk;
    }

    public CodeChunk codeChunk() {
        return codeChunk;
    }

    public ExportTableChunk exportTableChunk() {
        return exportTableChunk;
    }

    public FunctionTableChunk functionTableChunk() {
        return functionTableChunk;
    }

    public ImportTableChunk importTableChunk() {
        return importTableChunk;
    }

    public LiteralTableChunk literalTableChunk() {
        return literalTableChunk;
    }

    public LocalFunctionTableChunk localFunctionTableChunk() {
        return localFunctionTableChunk;
    }

    public StringTableChunk stringTableChunk() {
        return stringTableChunk;
    }

}
