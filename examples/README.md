# Examples

Two programs, one for each direction of the Java interop.

| Directory | What it shows |
| --- | --- |
| `gen-server` | Java embeds the runtime, starts an Erlang `gen_server` and calls it. |
| `java-service` | Erlang calls static methods of a Java class through `java:call_static/3`. |

Both use `org.jerlang.api.Node`, which owns the runtime: it starts the
schedulers, applies the options and, on `close()`, drains the object handle
table.

## Running them

```sh
mvn -B -DskipTests compile
bash examples/run.sh
```

`run.sh` needs `javac`, `erl` and `erlc` on the PATH. The first example runs
OTP's own `gen_server`, so it needs a real Erlang installation, not just the
runtime. The second one only needs a JDK, but `run.sh` compiles both.

Expected output:

```
--- Java calls Erlang ---
counter: 2
--- Erlang calls Java ---
hello, erlang
6 * 7 = 42
```

## Why the OTP root is passed in

`counter` is an ordinary `gen_server` callback module. The server that runs it
is OTP's `stdlib`, so `GenServerDemo` adds `stdlib` and `kernel` from the OTP
installation to the code path. A node that only uses the built-in modules does
not need that.
