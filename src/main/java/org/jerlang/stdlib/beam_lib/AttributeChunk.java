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

}
