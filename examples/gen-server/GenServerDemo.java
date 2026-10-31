import java.io.IOException;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.jerlang.CodePaths;
import org.jerlang.ModuleRegistry;
import org.jerlang.api.Bridge;
import org.jerlang.api.Node;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;

/**
 * Java embeds the runtime, starts an Erlang gen_server and calls it.
 *
 * <pre>
 * java -cp target/classes:. -Djerlang.otp.dir=/usr/lib/erlang GenServerDemo
 * </pre>
 *
 * The OTP root is needed because counter is an ordinary gen_server: the
 * server that runs it is OTP's stdlib, not a JErlang copy.
 */
public final class GenServerDemo {

    public static void main(String[] args) throws IOException {
        String otp = System.getProperty("jerlang.otp.dir");
        if (otp == null) {
            System.err.println("run with -Djerlang.otp.dir=/path/to/erlang");
            System.exit(2);
        }
        CodePaths.reset();
        ModuleRegistry.reset();
        CodePaths.addPatha(CodePaths.normalizePath("."));
        CodePaths.addPatha(ebin(otp, "stdlib"));
        CodePaths.addPatha(ebin(otp, "kernel"));

        try (Node node = Node.start()) {
            Tuple started = node.apply("counter", "start").toTuple();
            Term server = started.element(2);

            node.apply("counter", "increment", server);
            node.apply("counter", "increment", server);
            Object value = Bridge.toJava(node.apply("counter", "value", server));
            System.out.println("counter: " + value);

            node.apply("counter", "stop", server);
        }
    }

    /**
     * The ebin directory of an application in an OTP installation.
     */
    private static String ebin(String root, String application) throws IOException {
        Path lib = Paths.get(root, "lib");
        try (DirectoryStream<Path> directories = Files.newDirectoryStream(lib, application + "-*")) {
            for (Path directory : directories) {
                Path ebin = directory.resolve("ebin");
                if (Files.isDirectory(ebin)) {
                    return CodePaths.normalizePath(ebin.toString());
                }
            }
        }
        throw new IOException("no " + application + " in " + lib);
    }

}
