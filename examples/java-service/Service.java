/**
 * A plain Java class with static methods. service.erl reaches it through
 * java:call_static/3, so nothing on this side knows about Erlang.
 */
public final class Service {

    private Service() {
    }

    public static String greet(String name) {
        return "hello, " + name;
    }

    public static int scale(int value, int factor) {
        return value * factor;
    }

}
