package org.jerlang.erts.epmd;

import java.util.Collection;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class NodeRegistry {

    private Map<String, Node> nodes = new ConcurrentHashMap<>();
    private Map<String, Node> unreg = new ConcurrentHashMap<>();

    public void register(Node node) {
        nodes.put(node.nodeName(), node);
        unreg.remove(node.nodeName());
    }

    public void unregister(Node node) {
        nodes.remove(node.nodeName());
        unreg.put(node.nodeName(), node);
    }

    public String names() {
        StringBuilder stringBuilder = new StringBuilder();
        list(stringBuilder, nodes.values(), "name ", "\n");
        return stringBuilder.toString();
    }

    public String dump() {
        StringBuilder stringBuilder = new StringBuilder();
        list(stringBuilder, nodes.values(), "active name     ", ", fd = 0\n");
        list(stringBuilder, unreg.values(), "old/unused name ", ", fd = 0 \n");
        return stringBuilder.toString();
    }

    private static void list(StringBuilder sb, Collection<Node> nodes, String prefix, String suffix) {
        for (Node node : nodes) {
            sb.append(prefix).append(node.nodeName());
            sb.append(" at port ").append(node.portNo()).append(suffix);
        }
    }

}
