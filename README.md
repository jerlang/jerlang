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


## Contribution Process

This project uses the [C4.1 process](http://rfc.zeromq.org/spec:22) for all code changes.

> "Everyone, without distinction or discrimination, SHALL have an equal right to become a Contributor under the
terms of this contract."

### tl;dr

1. Check for [open issues](https://github.com/jerlang/jerlang/issues) or [open a new issue](https://github.com/jerlang/jerlang/issues/new) to start a discussion around a feature idea or a bug
2. Fork the [jerlang repository on Github](https://github.com/jerlang/jerlanga) to start making your changes
3. Write a test which shows that the bug was fixed or that the feature works as expected
4. Send a pull request
5. Your pull request is merged and you are added to the [list of contributors](https://github.com/jerlang/jerlang/graphs/contributors)

## License

[Apache 2.0 License](LICENSE)
