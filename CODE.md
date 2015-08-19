# Code

Some remarks regarding the coding style and
the organization of the source code.

## Erlang Terms

All Erlang terms are defined in `org.jerlang.type`.

When implementing Erlang/OTP in Java,
using these Erlang types is recommended.

There are some types which are not Erlang terms,
but can be pushed to the stack.
Those types are defined in `org.jerlang.type.stack`.

## Erlang Modules

Erlang modules are defined in `org.jerlang.<subsystem>` package,
where `<subsystem>` is one of:

* `erts`
* `kernel`
* `stdlib`

The module class name is the Erlang module name in CamelCase.

Implementation of module functions is moved to separated classes and
therefore follows the single responsibility principle.
A function implementation class is named `<module><function>` in CamelCase.
A function implementation class must provide a static `dispatch` method.

On startup, the ModuleRegistry automatically looks up `MethodHandles`
of the functions based on the static methods in the `Module` class.

## Java Coding Style

### Always use brackets

Always use brackets, even for single line `if` statements.
Using brackets make code blocks obvious to the reader.

### Asciidoc comments

* Comments, where necessary, must be in Asciidoc format.

* Javadoc (`@params` etc.) format shall be avoided.
