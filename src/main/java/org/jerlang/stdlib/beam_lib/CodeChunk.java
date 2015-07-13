package org.jerlang.stdlib.beam_lib;

/**
 * = The Code Chunk
 *
 * The Code Chunk is a mandatory chunk that stores the code for the module.
 * The Code Chunk is composed of a header followed by one or more function
 * definitions.
 *
 * == Header
 *
 * The Code Chunk Header is composed of 28 or more bytes.
 * This is the structure of the Code Chunk Header:
 *
 * [cols="1,1,6", options="header"]
 * |===
 * |Length
 * |Value
 * |Description
 *
 * |4 bytes
 * |`Code`
 * |Magic number indicating the Code Chunk.
 *
 * |4 bytes
 * |size
 * |Code Chunk length in bytes
 *
 * |4 bytes
 * |info-size
 * |Length of the information fields before code.
 *  This is for future expansion.
 *
 * |4 bytes
 * |version
 * |Instruction set version.
 *  This is 0 for OTP R5-R15.
 *  This will be incremented if the instructions are changed in incompatible
 *  ways (instructions renumbered, argument types changed, etc.)
 *
 * |4 bytes
 * |opcode-max
 * |The highest opcode used in the code section.
 *  This allows addition of new opcodes without incrementing the version
 *  of the BEAM file.
 *
 * |4 bytes
 * |labels
 * |The number of labels.
 *  This is a hint for the loader to help allocate the label table.
 *
 * |4 bytes
 * |function-count
 * |The number of functions contained in the Code Chunk.
 * |===
 *
 * NOTE: The value in the labels field holding the number of labels must be
 * 1 greater than the number of labels in the BEAM file
 * (remembering that labels are indexed from zero).
 * This is because the 0 label is reserved for the undefined location.
 * The compiler must set the labels value equal to the highest label it uses +1.
 *
 * == Code Definition Format
 *
 * The code is simply defined as a block of code.
 * Special instructions identify where functions insert into the code.
 * Each operation is coded according to the opcodes defined in the
 * (not yet existing) Opcode section.
 *
 * == Requirements
 *
 * A BEAM file loader should not load a BEAM file if:
 *
 * * It does not understand the instruction set version of the BEAM file
 * * The opcode-max is higher than the greatest opcode that the loader comprehends
 *
 * Based on:
 * https://synrc.com/publications/cat/Functional%20Languages/Erlang/BEAM.pdf
*/
public class CodeChunk extends Chunk {

    private int maxOpcode;
    private int labels;
    private int functions;

    public CodeChunk(int offset, int length) {
        super(ChunkId.CODE, offset, length);
    }

    public void setInfo(int maxOpcode, int labels, int functions) {
        this.maxOpcode = maxOpcode;
        this.labels = labels;
        this.functions = functions;
    }

    public int maxOpcode() {
        return maxOpcode;
    }

    public int labels() {
        return labels;
    }

    public int functions() {
        return functions;
    }

}
