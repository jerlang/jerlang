package org.jerlang.stdlib.beam_lib;

import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * = The Attribute Chunk
 *
 * The Attribute Chunk contains a list of attributes.
 */
public class AttributeChunk extends Chunk {

    private final List attributes;

    public AttributeChunk(Chunk chunk, Term attributes) {
        super(ChunkId.ATTR, chunk);
        if (attributes instanceof List) {
            this.attributes = (List) attributes;
        } else {
            throw new Error("attributes is not a list");
        }
    }

    public List attributes() {
        return attributes;
    }

    @Override
    public String toString() {
        StringBuilder stringBuilder = new StringBuilder();
        stringBuilder.append("{attribute_chunk,[");
        List list = attributes;
        while (list.length() > 0) {
            stringBuilder.append(list.head()).append(',');
            list = list.tail();
        }
        if (attributes.length() > 0) {
            stringBuilder.setLength(stringBuilder.length() - 1);
        }
        stringBuilder.append("]}");
        return stringBuilder.toString();
    }

}
