package org.jerlang.stdlib.beam_lib;

import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * = The Abstract Syntax Tree (AST) Chunk
 *
 * The AST Chunk contains the abstract syntax tree of the module.
 */
public class AbstractSyntaxTreeChunk extends Chunk {

    private final List ast;

    public AbstractSyntaxTreeChunk(Chunk chunk, Term ast) {
        super(ChunkId.ABST, chunk);
        this.ast = ast.toList();
    }

    public List ast() {
        return ast;
    }

    @Override
    public String toString() {
        StringBuilder stringBuilder = new StringBuilder();
        stringBuilder.append("{abstract_syntax_tree,[");
        List list = ast;
        while (list.length() > 0) {
            stringBuilder.append(list.head()).append(',');
            list = list.tail();
        }
        if (ast.length() > 0) {
            stringBuilder.setLength(stringBuilder.length() - 1);
        }
        stringBuilder.append("]}");
        return stringBuilder.toString();
    }

}
