package org.jerlang.stdlib.beam_lib;

/**
 * This class represents a BEAM file.
 */
public class BeamData {

    private final AtomChunk atomChunk;
    private final AttributeChunk attributeChunk;
    private final CodeChunk codeChunk;
    private final CompileInfoChunk compileInfoChunk;
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
        CompileInfoChunk compileInfoChunk,
        ExportTableChunk exportTableChunk,
        FunctionTableChunk functionTableChunk,
        ImportTableChunk importTableChunk,
        LiteralTableChunk literalTableChunk,
        LocalFunctionTableChunk localFunctionTableChunk,
        StringTableChunk stringTableChunk) {
        this.atomChunk = atomChunk;
        this.attributeChunk = attributeChunk;
        this.codeChunk = codeChunk;
        this.compileInfoChunk = compileInfoChunk;
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

    public CompileInfoChunk compileInfoChunk() {
        return compileInfoChunk;
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

    @Override
    public String toString() {
        StringBuilder stringBuilder = new StringBuilder()
            .append("{beam_data, [\n")
            .append("  ").append(atomChunk).append('\n')
            .append("  ").append(attributeChunk).append('\n')
            .append("  ").append(codeChunk).append('\n')
            .append("  ").append(compileInfoChunk).append('\n')
            .append("  ").append(exportTableChunk).append('\n')
            .append("  ").append(functionTableChunk).append('\n')
            .append("  ").append(importTableChunk).append('\n')
            .append("  ").append(literalTableChunk).append('\n')
            .append("  ").append(localFunctionTableChunk).append('\n')
            .append("  ").append(stringTableChunk).append('\n')
            .append("  ]}");
        return stringBuilder.toString();
    }
}
