import org.jerlang.api.Bridge;
import org.jerlang.api.Node;
import org.jerlang.api.NodeOptions;
import org.jerlang.type.Integer;
import org.jerlang.type.Str;

/**
 * Erlang calls Java: service.erl goes through java:call_static/3 into the
 * static methods of Service.
 *
 * <pre>
 * java -cp target/classes:. ErlangCallsJavaDemo
 * </pre>
 *
 * Reflection is opt-in, so the node has to ask for the java module before
 * service.erl can call anything.
 */
public final class ErlangCallsJavaDemo {

    public static void main(String[] args) {
        try (Node node = Node.start(NodeOptions.builder().reflection(true).build())) {
            Object greeting = Bridge.toJava(node.apply("service", "greet", Str.of("erlang")));
            Object scaled = Bridge.toJava(node.apply("service", "scale",
                Integer.of(6), Integer.of(7)));
            System.out.println(greeting);
            System.out.println("6 * 7 = " + scaled);
        }
    }

}
