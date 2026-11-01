# Operating a node

Milestone 0.9 covers running and debugging a node like a service: logging,
metrics, error reports, graceful shutdown and distribution hardening. This page
documents the parts that exist; the roadmap in [roadmap.md](roadmap.md) has the
rest.

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

## Error reports

An uncaught exception terminates its process and prints the Erlang/OTP style
`ERROR REPORT`, with the process id, the exit value and the captured stack
frames (`org.jerlang.vm.CrashReport`). An uncaught `exit` and a process killed by
an exit signal print nothing, as in OTP.
