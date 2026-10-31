# Java interoperability

JErlang runs on the JVM, so Erlang code calls Java through reflection and Java
code calls Erlang through `org.jerlang.api.Bridge`. This page defines the value
mapping and the handle rules. The tables are the 0.8 compatibility contract:
changing a row needs a version bump.

## Erlang to Java

| Erlang term | Java type | Notes |
|-------------|-----------|-------|
| integer that fits in 32 bits | `java.lang.Integer` | |
| integer that fits in 64 bits | `java.lang.Long` | |
| larger integer | `java.math.BigInteger` | |
| float | `java.lang.Double` | |
| atom `true` / `false` | `java.lang.Boolean` | |
| atom `undefined` | `null` | |
| any other atom | `java.lang.String` | Returning a string becomes an Erlang string, not the original atom. |
| string (charlist) | `java.lang.String` | `Str` and a list of character integers both convert. |
| binary | `byte[]` | |
| list | `java.util.List` | An `ArrayList`; the nil list becomes an empty list. |
| tuple | `Object[]` | |
| map | `java.util.Map` | A `LinkedHashMap`; keys and values are converted recursively. |
| object handle | the live Java object | Handles are references created by `java:new`, not values. |

## Java to Erlang

| Java type | Erlang term | Notes |
|-----------|-------------|-------|
| `null` | atom `undefined` | |
| `java.lang.Boolean` | atom `true` / `false` | |
| `java.lang.String` | string | |
| `java.lang.Character` | one-character string | |
| `Byte`, `Short`, `Integer`, `Long` | integer | |
| `java.math.BigInteger` | integer | |
| `Float`, `Double` | float | |
| `byte[]` | binary | |
| `Object[]` | tuple | |
| `java.util.Collection` | list | |
| `java.util.Map` | map | Keys and values are converted recursively. |
| any other object | object handle | Use `java:release/1` when the Erlang side is done. |

Java does not distinguish integer widths in Erlang, so `int`, `long` and
`BigInteger` all become one Erlang integer type and the width is chosen again
on the way back.

## Arguments and overload resolution

An argument is converted to the declared parameter type before a method or
constructor is chosen. For an Erlang integer the preference is `int`, then the
other integral types, then floating point. When several overloads accept the
arguments, the one with the lowest conversion distance wins; an `Object`
parameter gets the generic conversion above. A `char` parameter accepts a
one-character string or a code point, and an enum parameter accepts an atom
with the constant's name.

Java exceptions surface as an Erlang `error` with reason
`{java_exception, ClassName, Message}`. An Erlang exit does not stop the JVM.

## Object handles

`java:new/2` and any Java value that has no value mapping above are returned as
an object handle. One handle exists per Java object identity: asking again for
the same object returns the same reference. Handles are node-local and are not
Erlang terms that can be sent to another node.

The table belongs to the virtual machine, not to the bridge, so a handle lives
exactly as long as the node that created it. It starts empty when the node
starts and is drained when the node shuts down. It is bounded by
`NodeOptions.maxHandles`, default 100 000; creating a handle past the cap
raises `system_limit`, the reason Erlang already uses for an exhausted runtime
resource. `{java_exception, _, _}` stays reserved for exceptions thrown by
Java code.

### Release policy

* `java:release/1` releases early. A call on a released handle raises an
error, and the reference is never reused for another object.
* `Node.close()`, and any VM shutdown, drains the whole table, so it is the
  catch-all for handles a caller forgot.
* There is no finalizer and no `Cleaner`: release happens explicitly or at
  shutdown, never because the garbage collector ran. An unreleased handle
  keeps its Java object reachable for the lifetime of the node.
* Raising `NodeOptions.maxHandles` does not make forgetting a release safe; it
  only moves the `system_limit` further away.

## Decided but not yet implemented

These decisions are recorded before the code lands, so the contract can be
reviewed on its own. Everything above describes what the code does today; this
section describes what 0.8 must make it do.

### Reflection is opt-in

`NodeOptions.reflection(boolean)` defaults to `false`. The `java` module is
registered on node start and unregistered on close, so a node that does not ask
for reflection has no `java:new/2` and cannot reach arbitrary JVM classes.
`Bridge` and the value mapping are unaffected, because neither needs the module.

### Embedding API

`org.jerlang.api.Node` implements `AutoCloseable` and owns the node:

```java
Node node = Node.start(NodeOptions.builder().reflection(true).build());
node.registerFunction("my_mod", "echo", 1, args -> args[0]);
Term reply = node.apply("gen_server", "call", pid, request, Integer.of(5000));
Term out = node.apply(anErlangFun, Integer.of(41));
node.close();
```

`apply` takes both a module name and an Erlang fun. `registerFunction` makes a
Java lambda callable from Erlang through the normal module registry. `Bridge`
stays as it is: a static convenience over a default node.

### Deterministic overload resolution

Conversion distance picks the candidate, as it does today. Ties are broken by
parameter specificity and then by sorted signature, so the same call picks the
same method on every JVM. Today a tie is settled by `Class#getMethods`
iteration order, which the JDK does not specify. Varargs are supported for a
trailing `Object...` parameter only.

### Compatibility gate

`mvn verify` runs japicmp 0.26.5 against a baseline jar checked in under
`src/api-baseline/`, with `accessModifier public`,
`breakBuildOnSourceIncompatibleModifications` and
`breakBuildBasedOnSemanticVersioning`. Because the project is still `0.x`,
`breakBuildBasedOnSemanticVersioningForMajorVersionZero` must also be set or the
gate does nothing. The baseline is refreshed only when the version is bumped.

## Stability

`Bridge.toTerm/1` and `Bridge.toJava/1` are the conversion entry points for
embedding. `TypeMappingTest` has one test per row above, and a change to a row
must come with a version bump.
