package org.jerlang.api;

import org.jerlang.jvm.HandleTable;

/**
 * = Node options
 *
 * What an embedded node is allowed to do. The defaults are the safe ones: a
 * node that says nothing has no reflection and a handle table of
 * {@link HandleTable#DEFAULT_MAX_HANDLES}.
 *
 * ----
 * Node node = Node.start(NodeOptions.builder().reflection(true).build());
 * ----
 */
public final class NodeOptions {

    private final boolean reflection;
    private final int maxHandles;
    private final int schedulers;

    private NodeOptions(Builder builder) {
        this.reflection = builder.reflection;
        this.maxHandles = builder.maxHandles;
        this.schedulers = builder.schedulers;
    }

    public static NodeOptions defaults() {
        return builder().build();
    }

    public static Builder builder() {
        return new Builder();
    }

    /**
     * Whether the node registers the java module. Off by default: without it
     * Erlang code on this node cannot reach arbitrary JVM classes.
     */
    public boolean reflection() {
        return reflection;
    }

    /**
     * How many object handles the node may hand out at once.
     */
    public int maxHandles() {
        return maxHandles;
    }

    /**
     * How many schedulers the node starts, or 0 to follow the host's
     * processor count.
     */
    public int schedulers() {
        return schedulers;
    }

    public static final class Builder {

        private boolean reflection = false;
        private int maxHandles = HandleTable.DEFAULT_MAX_HANDLES;
        private int schedulers = 0;

        private Builder() {
        }

        public Builder reflection(boolean reflection) {
            this.reflection = reflection;
            return this;
        }

        public Builder maxHandles(int maxHandles) {
            if (maxHandles < 1) {
                throw new IllegalArgumentException("maxHandles must be positive: " + maxHandles);
            }
            this.maxHandles = maxHandles;
            return this;
        }

        public Builder schedulers(int schedulers) {
            if (schedulers < 0) {
                throw new IllegalArgumentException("schedulers must not be negative: " + schedulers);
            }
            this.schedulers = schedulers;
            return this;
        }

        public NodeOptions build() {
            return new NodeOptions(this);
        }
    }

}
