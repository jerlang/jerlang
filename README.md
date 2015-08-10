# JErlang

[Erlang/OTP](https://github.com/erlang/otp) on the [Java Virtual Machine](https://en.wikipedia.org/wiki/Java_virtual_machine) (JVM)

[![Build Status](https://img.shields.io/travis/jerlang/jerlang.svg?branch=master&style=flat)](https://travis-ci.org/jerlang/jerlang)
[![Dependency Status](https://www.versioneye.com/user/projects/55acc9853065350023000481/badge.svg?style=flat)](https://www.versioneye.com/user/projects/55acc9853065350023000481)
[![Bountrysource](https://img.shields.io/bountysource/team/jerlang/activity.svg?style=flat)](https://www.bountysource.com/teams/jerlang)



## Roadmap

|Version|Focus           |Milestone                                         |Status|
|:-----:|----------------|--------------------------------------------------|:----:|
|0.1    |File Format     |BEAM files can be parsed                          |:heavy_check_mark:|
|0.2    |Proof of Concept|A simple "hello world" can be interpreted         |:heavy_check_mark:|
|0.3    |Interpreter     |Most single-node applications can be interpreted  ||
|0.4    |Network         |Most multi-node applications can be interpreted   ||
|0.5    |Compiler        |BEAM files can be compiled to Java bytecode       ||
|0.6    |Scheduler       |Soft realtime support                             ||
|0.7    |Compatibility   |JErlang is fully compatible with native Erlang/OTP||
|0.8    |Documentation   |The documentation is complete and awesome         ||
|0.9    |Performance     |Performance is comparable to native Erlang/OTP    ||
|1.0    |Stability       |JErlang is production ready                       ||

## Contribution

All contributions are welcome!

This project uses the [C4.1 process](http://rfc.zeromq.org/spec:22)
for all code changes.

> "Everyone, without distinction or discrimination,
> SHALL have an equal right to become a Contributor
> under the terms of this contract."

### tl;dr

1. Check for [open issues](https://github.com/jerlang/jerlang/issues) or
[open a new issue](https://github.com/jerlang/jerlang/issues/new) to start
a discussion around a feature idea or a bug
2. Fork the [JErlang repository on Github](https://github.com/jerlang/jerlang)
to start making your changes
3. Write a test which shows that the bug was fixed or that the feature works
as expected
4. Send a pull request. If it does not break the build, it will be merged.

## See also

* [erjang: A JVM-based Erlang VM](https://github.com/trifork/erjang)
* [JErlang: Erlang with Joins](http://www.doc.ic.ac.uk/~susan/jerlang/)

## License

[Apache 2.0 License](LICENSE)
