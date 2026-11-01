# Operating a node

Milestone 0.9 covers running and debugging a node like a service: logging,
metrics, error reports, graceful shutdown and distribution hardening. This page
documents what a deployment can rely on, and where the limits are.

## Metrics

`org.jerlang.api.Node.metrics()` returns a `org.jerlang.vm.Metrics` snapshot of
the running node. `Metrics.snapshot()` does the same for the process-wide VM when
there is no `Node` at hand.

The snapshot is a sample, not a stream. Each field is read from the live process
registry in turn, so a scrape that overlaps with processes starting or dying can
be slightly inconsistent, and no counter on the hot path exists to keep it
consistent. This is the same trade `erlang:statistics/1` makes.

| Field                        | Meaning                                                                 |
|------------------------------|-------------------------------------------------------------------------|
| `processes()`                | Live processes, not counting ports.                                     |
| `ports()`                    | Live ports.                                                             |
| `runQueueDepth()`            | Processes and ports waiting on all schedulers together.                 |
| `runQueueDepth(i)`           | Waiting work on scheduler `i`.                                          |
| `reductions()`               | Reductions executed by the processes that are alive at the reading.      |
| `mailboxes()`                | Messages waiting in all mailboxes together.                             |
| `schedulerCount()`           | Scheduler threads in this VM.                                           |
| `utilisation()`              | Busy fraction of all schedulers, 0.0 to 1.0.                            |
| `utilisation(i)`             | Busy fraction of scheduler `i`.                                         |
| `uptimeMillis()`             | Milliseconds since the VM started, or 0 while it is not running.        |

Reductions are summed over the live processes, so the number is **not**
cumulative for the node's lifetime. It answers "how much work is this node
carrying now", not "how much work has it done since start". A process that
terminates takes its count with it, which is why a scrape can go down as well as
up.

Utilisation is measured per scheduler loop iteration: time spent executing work
divided by time spent executing plus time parked with an empty queue. A scheduler
that is blocked inside a BIF that waits for another process counts as busy, so a
node with a blocked scheduler and an idle one reports around 0.5.

The numbers are only collected while the VM runs. After `Node.close()`,
`uptimeMillis()` and the process counts are zero.

### Text format

`toText()` renders the snapshot in one line per metric, `name value`, so it can
be scraped by anything that reads a key and a number. Per-scheduler readings
carry the scheduler index as a label:

```
jerlang_processes 3
jerlang_ports 0
jerlang_run_queue 1
jerlang_reductions 14872
jerlang_mailbox_messages 0
jerlang_schedulers 8
jerlang_uptime_millis 1043
jerlang_scheduler_utilisation 0.2137
jerlang_scheduler_queue{index="0"} 0
jerlang_scheduler_utilisation{index="0"} 0.3125
```

Utilisation is printed with four decimal places, in the root locale, so a
comma-decimal JVM does not change the file. Counts are plain integers.

## Logging

OTP's `logger` module is backed by `java.util.logging`, so an embedding
deployment configures one logging system instead of two. Every report is
published to the logger named `org.jerlang.otp`; `org.jerlang.stdlib.Logger.jul()`
returns it.

The OTP levels map like this:

| OTP level   | java.util.logging level | Value |
|-------------|-------------------------|-------|
| `debug`     | `FINE`                  | 500   |
| `info`      | `INFO`                  | 800   |
| `notice`    | `NOTICE` (custom)       | 850   |
| `warning`   | `WARNING`               | 900   |
| `error`     | `ERROR` (custom)        | 950   |
| `critical`  | `CRITICAL` (custom)     | 1000  |
| `alert`     | `ALERT` (custom)        | 1050  |
| `emergency` | `EMERGENCY` (custom)    | 1100  |

The logger's level is `ALL` by default, so a node that configures nothing prints
what it always printed. Setting the level filters reports before they are
formatted *and* makes `logger:allow/2,3` answer accordingly, which is what the
`?LOG_*` macros of `logger.hrl` consult:

```java
Logger.jul().setLevel(Level.WARNING);   // info and notice are dropped
Logger.jul().addHandler(myHandler);     // structured sink, file, test spy
```

Records are `org.jerlang.stdlib.Logger.Event`, a `LogRecord` that also carries
`otpLevel()` (the level atom) and `metadata()` (the map the caller passed;
`module` and `mfa` are also copied onto the record's source class and method).

Two limits: the default handler writes the OTP-shaped report to standard output,
and the OTP handler chain is not implemented — `logger:add_handler/3`,
`logger:set_primary_config/2` and the other configuration functions are accepted
and ignored. Filters and formatters are likewise not applied.

## Error reports and process state

An uncaught exception terminates its process and prints an Erlang/OTP style
report: the header, the process id, the exit value with its captured stack
frames, and the state the process was in when it died.

```
=ERROR REPORT==== 2026-11-01::09:12:44.512300 ===
Error in process <0.34.0> with exit value:
{{badmatch,2},[{crash_demo,crash_now,1,[{file,"crash_demo.erl"},{line,19}]}]}
Process state:
  pid: <0.34.0>
  reductions: 183
  mailbox: 1 message
  messages: [hello]
  links: [<0.9.0>]
```

Mailbox contents are previewed to five messages; a crash report is a line in a
log, not a dump file. There is no full VM crash dump writer yet: when the JVM
itself dies, the JVM's own error file is the artefact.

An uncaught `exit` and a process killed by an exit signal print nothing, as in
OTP.

## Graceful shutdown

`Node.close()` gives the work that is already running a chance to finish before
the schedulers stop:

1. the node is marked closed, so new `apply` and `registerFunction` calls fail
   immediately with `IllegalStateException` instead of racing the shutdown;
2. the distribution listener stops and every connection is closed, so no new
   remote work arrives;
3. running applications are stopped newest first, so an application is always
   gone before the applications it was started on top of. `kernel` and `stdlib`
   are permanent, as in OTP;
4. the run queues are drained: the shutdown waits until no scheduler has a
   process or port waiting, bounded by `-Djerlang.shutdown.drain.ms`
   (default 1000) so a process that never finishes cannot keep the JVM up;
5. the schedulers are interrupted, the timer wheel stops, and the process
   registry and object handle table are cleared.

`VirtualMachine.shutdown()` performs the same sequence; the drain window is the
only part that waits. A test suite that starts and stops a VM per test pays
nothing for it while the queues are empty, because the drain returns as soon as
they are.

## Distribution

A node that is named (`-name`/`-sname`, or `VirtualMachine.setNode/1`) accepts
connections from other nodes and opens outgoing ones. There is one connection
per peer, and it carries messages, links, exits, monitors and spawn requests.
Peers are found through EPMD, as in OTP.

### Reconnection

When a peer disappears, every link and monitor to it gets `noconnection`, as in
OTP. Unlike OTP, a node that *was* connected is then retried automatically: 12
attempts, waiting 100 ms on the first and doubling up to 5 s, so about half a
minute in total. A successful connection fires the usual
`net_kernel:monitor_nodes/1` delivery of `{nodeup, Node}`. Retries stop when the
peer answers, when the node shuts down, or after the last attempt; talking to
the node again — or `net_kernel:connect_node/1` — connects on demand as before.

Set `-Djerlang.dist.reconnect=false` to keep OTP's behaviour of never
reconnecting on its own. An explicit `erlang:disconnect_node/1` is a decision,
not a loss, and is not retried.

Reconnection restores the connection, not the processes behind it: links and
monitors that were reported as `noconnection` are not re-established, because
they belong to the processes that saw them fail. A service that needs those
relationships back has to rebuild them when it sees `nodeup`.

### rex compatibility

JErlang's remote calls between JErlang nodes travel as `SPAWN_REQUEST` and
`SPAWN_REPLY` control messages, not as `rex:call` messages. A JErlang node is
therefore not a drop-in `rex` peer: `rpc:call/4` from an OTP node to a JErlang
node is not supported, and JErlang's `rpc` module is the one to use between
JErlang nodes. This is a decision, not a pending feature: implementing rex would
mean carrying OTP's `rex` gen_server semantics on this side.

### TLS

Distribution is plain TCP with the cookie handshake, as OTP's default. TLS
distribution is not implemented. A node should not be reachable from an
untrusted network; put it behind a network boundary, or use distribution only
between hosts you control.

### Limits

* One connection per peer; the connection carries no distribution header and no
  atom cache, so both sides encode each term with their own version byte.
* A packet is capped at 64 MiB (`DistConnection.MAX_PACKET`).
* The tick time defaults to 60 seconds and can be set with
  `-Djerlang.dist.ticktime`.
* Hidden nodes connect and are visible to `erlang:nodes(connected)`, not to
  `erlang:nodes()`.
* There is one VM per JVM process: two nodes in one JVM are not supported.

## What the 0.9 exit test covers

`OperationsTest` runs a node under concurrent calls while scraping metrics,
checks that no call is lost, and closes the node cleanly: no process, port or
uptime is left behind. The reconnection policy is covered by
`DistReconnectTest`, which exercises the retry bookkeeping and backoff without
needing a second machine. A multi-host end-to-end test that kills a peer and
waits for `nodeup` is not part of the suite yet.
