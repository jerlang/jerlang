# jerlang

Erlang on the JVM

## Goal

This project aims to implement Erlang on the JVM.

#### What is wrong with [erjang](https://github.com/trifork/erjang)?

Nothing.
It is a great project, which is actually able to run Erlang on the JVM, but it has some flaws:

* Development seems to have been stopped in mid-2014.
* Supported OTP version is R16B01, but latest Erlang/OTP is 18.0.
* Does not support Java 8
* No seperation between code representation and code generation.
* Inconsistent naming (EObject instead of Term, etc.)
* Outdated dependencies
* Build is too complicated (uses e.g. ant and perl)

## Status

* Very early stage of development.
* Not able to run Erlang yet.

## License

[Apache 2.0 License](LICENSE)
