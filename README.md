# Interim

**Interim** is a statically-typed, low-level dialect of Lisp featuring
compile-time, GC-free memory management.

Interim is a technology demonstrator for compile-time memory management
using [regions][region]. As of 2018, the only major language with compile-time
memory management is [Rust][rust], which is notoriously tough to learn. Interim
was built to prove that sound, GC-free, compile-time memory management can be
implemented in a way that's both simple to build and easy to learn and use.

## Examples

## Building

You need MLton, git and make to build Interim. On Ubuntu:

~~~bash
$ sudo apt-get install mlton git make
~~~

Then:

~~~bash
$ make interim
~~~

### Dependencies

## Bibliography

## License

Copyright 2018 Fernando Borretti.

Licensed under the GPLv3 license. See the COPYING file for details.

The only dependency is [Parsimony][parsimony], a parser combinator library.

[region]: http://www.cs.umd.edu/projects/PL/cyclone/scp.pdf
[rust]: https://www.rust-lang.org/en-US/
[parsimony]: https://github.com/eudoxia0/parsimony
