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

The only dependency is [Parsimony][parsimony], a parser combinator library.

## Bibliography

- Grossman, Dan, Greg Morrisett, Trevor Jim, Michael Hicks, Yanling Wang, and
  James Cheney. ["Region-based memory management in Cyclone."][region-cyclone]
  ACM Sigplan Notices 37, no. 5 (2002): 282-293.

- Swamy, Nikhil, Michael Hicks, Greg Morrisett, Dan Grossman, and Trevor
  Jim. ["Safe manual memory management in Cyclone."][safe-mem] Science of
  Computer Programming 62, no. 2 (2006): 122-144.

- Gay, David, and Alex
  Aiken. [Memory management with explicit regions][explicit]. Vol. 33,
  no. 5. ACM, 1998.

## License

Copyright 2018 Fernando Borretti.

Licensed under the GPLv3 license. See the COPYING file for details.

[rust]: https://www.rust-lang.org/en-US/
[parsimony]: https://github.com/eudoxia0/parsimony

[region-cyclone]: https://www.cs.umd.edu/projects/cyclone/papers/cyclone-regions.pdf
[safe-mem]: http://www.cs.umd.edu/projects/PL/cyclone/scp.pdf
[explicit]: http://titanium.cs.berkeley.edu/papers/gay-thesis.pdf
