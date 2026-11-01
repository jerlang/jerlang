# JErlang

An Erlang runtime for the [Java Virtual Machine](https://en.wikipedia.org/wiki/Java_virtual_machine) (JVM).

[![Tests](https://github.com/slayyyyer/jerlang/actions/workflows/tests.yml/badge.svg?branch=master)](https://github.com/slayyyyer/jerlang/actions/workflows/tests.yml)

> **Fork notice:** this project began as a fork of [jerlang/jerlang](https://github.com/jerlang/jerlang) and is continued in this repository.

JErlang reads the BEAM files that `erlc` produces and interprets them on the JVM. Erlang processes, message passing, links, monitors and distribution are implemented in Java, and Java code is one reflective call away instead of a separate node, a port or an external term format round trip.

## Intended goals

**Run Erlang where the Java code is.** The primary target is a single JVM service that hosts an Erlang control plane next to Java code: supervision trees, message-driven workers and a Java API, in one process, with calls in both directions. The same node joins an Erlang/OTP cluster over the distribution protocol, so Erlang and Java can be deployed independently.

**Use OTP's own library as the compatibility bar.** The measure of correctness is not "how many opcodes exist" but "does `gen_server` from an OTP installation start and behave". Running OTP's `gen_server`, `supervisor`, `application`, `proc_lib` and `logger` unmodified is the bar for the standard library work.

**Stay honest about the boundary.** JErlang is an interpreter, not a port of BEAM's C internals. There are no NIFs, no hot code upgrade, no OTP release handling and no bytecode compiler today. Those are decisions with reasons in [docs/roadmap.md](docs/roadmap.md), not forgotten features.

The north-star metric is the share of a checked-in module corpus that loads and runs correctly against OTP 29. Opcode counts are a means, not the goal.

## Status

### What works today

**Language and BEAM loading**

- BEAM files compiled by OTP 29 load, including the literal, line, atom, export, import and lambda tables.
- All 161 non-deprecated OTP 29 opcodes have handlers: calls, tail calls, funs, `try`/`catch`, stack frames, maps, tuples, list comprehensions, records and record updates, and exceptions with real stacktraces.
- Binary matching and construction work, including bit-level patterns, signed and little endian fields, tail capture, UTF segments, and string-table-backed string segments.
- A checked-in corpus of modules compiled by OTP 29 runs in CI.
- All 1366 modules shipped with OTP 29.1.2 load, and every instruction in them has a handler. A separate CI job measures this rate, so the number is checked, not claimed.

**OTP**

- OTP's own `gen_server`, `supervisor`, `application`, `proc_lib`, `logger` and `code` run unmodified from an OTP 29 installation.
- The application controller handles included applications, start phases and dead-tree bookkeeping, including stopping an included application when its parent's tree dies on its own.

**Distribution and networking**

- Distribution in both directions: handshake, external term format (including funs, ports, and compressed terms on receive), links, monitors, remote spawn, `erpc` and `rpc`, and node monitors. Verified against a real OTP 29 peer.
- `gen_tcp`, `gen_udp` and `inet`, including IPv6 and multicast.

**Runtime and concurrency**

- Reduction accounting and preemptive scheduling for CPU-bound Erlang loops in their outermost activation. A process gets 1,000 instructions per scheduler slice, `process_info(reductions)` reports the count, and `SchedulerFairnessTest` keeps p99 timer wakeup under 50 ms with twice as many CPU-bound processes as schedulers.
- Cross-module tail calls are trampolined through the interpreter loop, so an alternating `A:burn/1`/`B:burn/1` loop yields at instruction boundaries instead of growing the JVM stack.
- A single timer wheel for `receive ... after`, `erlang:send_after`, `erlang:start_timer` and `timer:send_after`, with `cancel_timer` and `read_timer`.
- `process_info/1,2` reports the operator fields, including memory, current function, `max_heap_size` and `message_queue_data`, and `process_flag/2` handles priority, heap size and queue data.
- Priority queues run max, high, normal and low work in that order.
- `erlang:trace/3` and `erlang:trace_pattern/3` emit call, send and receive events to a tracer.

**ETS**

- `set` and `ordered_set` tables, with named tables and `{keypos, N}`.
- `new`, `insert`, `lookup`, `delete`, `tab2list`, `update_counter`, `all`, `whereis`, `match`, `match_object`, `select` and `give_away` work.
- A table is destroyed when its owner terminates, `give_away` moves the ownership first, and `update_counter` is atomic across processes.
- Match specs support the whole pattern language, bodies and the common guards.

**Code server**

- `-pa`, `-pz` and `ERL_LIBS` build a search path, and `code:which/1`, `code:lib_dir/1,2`, `code:priv_dir/1`, `code:root_dir/0`, `code:load_file/1`, `code:is_loaded/1`, `code:get_path/0`, `code:add_patha/1`, `code:add_pathz/1` and `code:del_path/1` report and use it.
- Modules load from any directory on the path, not just the working one.
- Sticky directories, `code:replace_path/2` and `code:ensure_modules_loaded/1` work.

**Java interop**

- A reflective Java bridge (`java:new`, `java:call`, fields, release) and an embedding entry point for Java calling Erlang.

**Tests**

- A 507-test suite. The unit tests need no Erlang and run on every push and pull request. Ten tests drive OTP's own modules and run in a separate CI job with an OTP 29 installation; a further 15 are opt-in external or machine tests (the live peer suite, the pass-rate run and the benchmarks).

### What does not work yet

- **Native records**, the experimental OTP 29 data type declared as `-record #name{...}`. Type tests always fail and construction raises a named error; ordinary tuple records are fully supported.
- **ETS gaps:** no `bag`, `duplicate_bag`, `match_delete`, `select_delete` or heir support. In match specs, `is_map_key` and arithmetic guards are not implemented.
- **Standard library coverage:** `gen_statem` and the wider standard library are not covered.
- **`application_master`** is not modelled: distributed takeover and automatic restart of a crashed permanent application are missing.
- **Fair scheduling across nested non-tail calls.** A CPU-bound loop in a nested cross-module call is not preempted until that call returns; cross-module tail calls are trampolined and do yield.
- **Timer precision.** Timers are bounded by the timer wheel's 1 ms tick and the scheduler wakeup, not by an OS timer pool; a 10 ms timer is checked against a 250 ms bound.
- **Code loading.** The loader is sequential because the atom table and module registry are not thread safe, and OTP boot scripts are not interpreted.

[docs/beam-compatibility.md](docs/beam-compatibility.md) keeps the per-opcode notes and documents the one remaining language-feature gap.

## Roadmap

| Version | Focus                   | Exit test                                                    | Status  |
|:-------:|-------------------------|--------------------------------------------------------------|:-------:|
| 0.1     | File format             | BEAM files parse                                             | Done    |
| 0.2     | Proof of concept        | A simple "hello world" is interpreted                        | Done    |
| 0.3     | Language                | Most BEAM opcodes are implemented                            | Done    |
| 0.4     | Network                 | Distribution and sockets work against OTP 29                 | Done    |
| 0.5     | Language completeness   | A corpus of constructs runs with no unsupported opcode       | Done    |
| 0.6     | Standard library boot   | Unmodified `gen_server`/`supervisor` start via `application` | Done    |
| 0.7     | Runtime and concurrency | Preemption, timer bounds, no leaked processes under stress   | Done    |
| 0.8     | Interop and embedding   | Java↔Erlang examples in CI and a frozen API                  | Done    |
| 0.9     | Cluster and operations  | Metrics, reconnection and graceful shutdown under load       | In progress |
| 1.0     | Release                 | Benchmarks against OTP 29, 24 hour soak, fuzzing, artifact   | Planned |

The full plan, including what was dropped from the original roadmap and why, is in [docs/roadmap.md](docs/roadmap.md).

## Java interoperability

Erlang code runs on the JVM itself, so calling Java is a direct reflective call: no external term format, no sockets, no separate node.

```erlang
Builder = java:new("java.lang.StringBuilder", []),
java:call(Builder, "append", ["hello"]),
"hello" = java:call(Builder, "toString", []),
java:release(Builder).
```

| Function                                                                 | Purpose                                                 |
|--------------------------------------------------------------------------|---------------------------------------------------------|
| `java:new(Class, Args)`                                                  | Call a constructor, returns an object handle            |
| `java:call(Object, Method, Args)`                                        | Call an instance method                                 |
| `java:call_static(Class, Method, Args)`                                  | Call a static method                                    |
| `java:get(Object, Field)` / `java:set(Object, Field, Value)`             | Instance fields                                         |
| `java:get_static(Class, Field)` / `java:set_static(Class, Field, Value)` | Static fields                                           |
| `java:release(Object)`                                                   | Drop the handle, allowing the JVM to collect the object |

Values are converted on the way in and out: integers to `int`/`long`/`short`/`byte`/`double`/`float`/`BigInteger`/`BigDecimal`, floats to `double`/`float`, strings and atoms to `String`, binaries to `byte[]`, lists to collections or arrays, tuples to `Object[]`, `true`/`false` to `boolean`, and `null` back to the atom `undefined`. Java exceptions surface as `error({java_exception, ClassName, Message})`.

The reverse direction is `org.jerlang.api.Bridge`:

```java
Term result = Bridge.call("lists", "reverse", List.of(Integer.of(1), Integer.of(2)));
```

The type mapping is specified row by row in [docs/java-interop.md](docs/java-interop.md) and covered by `TypeMappingTest`. Object handles are bounded per VM and drained on shutdown, and `org.jerlang.api.Node` embeds a node that owns them.

## Compiled Erlang

JErlang does not compile Erlang source. Compile it with `erlc` and load the resulting `.beam` from the working directory or from a directory that `-pa`, `-pz` or `ERL_LIBS` puts on the code path:

```sh
erlc -o src/test/resources src/test/erl/bridge_demo.erl
```

Modules built by current OTP releases need the loader support described in [docs/beam-compatibility.md](docs/beam-compatibility.md).

## Build and test

JErlang builds with Maven. CI runs it on JDK 25; the compiler targets Java 21, so any JDK from 21 up can build it and the jar runs on 21 and newer. Building with `--release` means an accidental use of a newer JDK's API is a compile error instead of a `NoSuchMethodError` at run time. The unit tests need no installed Erlang.

```sh
mvn test
mvn package
java -jar target/jerlang-<version>.jar
```

The live interoperability tests are opt-in because they need a peer node. Start an OTP 29 node with the cookie from [docs/distribution.md](docs/distribution.md), then:

```sh
mvn test -Dtest=OtpInteropTest -Djerlang.dist.otp=true
```

The benchmarks are opt-in too, because they measure the machine rather than the build:

```sh
mvn -Dtest=BenchTest -Djerlang.bench=true test
```

Coverage and mutation testing are opt-in as well:

```sh
mvn jacoco:prepare-agent test jacoco:report
mvn pitest:mutationCoverage
```

`mvn pitest:mutationCoverage` mutates every class under `org.jerlang` and takes a long time, so point it at the package under change while iterating:

```sh
mvn pitest:mutationCoverage -DtargetClasses=org.jerlang.type.Nil -DtargetTests=org.jerlang.type.ListTest
```

## Documentation

- [docs/java-interop.md](docs/java-interop.md): the value mapping, the bridge rules and the embedding API
- [docs/roadmap.md](docs/roadmap.md): milestones and what was dropped from the original plan
- [docs/beam-compatibility.md](docs/beam-compatibility.md): opcode and language coverage
- [docs/distribution.md](docs/distribution.md): distribution protocol and sockets
- [docs/benchmarks.md](docs/benchmarks.md): the recorded performance baseline

## Contribute

JErlang uses the [C4.1 (Collective Code Construction Contract) process](http://rfc.zeromq.org/spec:22) for contributions. Pull requests are handled in this repository.

## See also

- [jerlang/jerlang](https://github.com/jerlang/jerlang): the original project this repository was forked from
- [erjang: A JVM-based Erlang VM](https://github.com/trifork/erjang)
- [JErlang: Erlang with Joins](http://www.doc.ic.ac.uk/~susan/jerlang/)

## License

[Apache 2.0 License](LICENSE)
