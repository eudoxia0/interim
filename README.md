# Interim

**Interim** is a statically-typed, low-level dialect of Lisp featuring
compile-time, GC-free memory management.

Interim is a technology demonstrator for compile-time memory management
using [regions][region]. As of 2018, the only major language with compile-time
memory management is [Rust][rust], which is notoriously tough to learn. Interim
was built to prove that sound, GC-free, compile-time memory management can be
implemented in a way that's both simple to build and easy to learn and use.

## Overview of Regions

Memory safety is achieved by preventing three classes of errors:

- `NULL pointers`
- Double `free()`.
- Use after `free()`.

Preventing `NULL` pointers is easy: we use an option type (called `Maybe` in
Haskell). In the case of Interim, we have a special pointer type called
`nullable` which represents a pointer which is potentially `NULL`. The
`nullable/case` construct can be used to extract a never-`NULL` pointer in a
safe way.

Preventing double and use after `free()` errors is harder, and this is where
regions come in.

Pointers are tagged with the region they belong to.

## Examples

### Hello World

~~~lisp
(defun main () i32
  (println "Hello, world!")
  0)
~~~

### Fibonacci

~~~lisp
(defun fib ((n i32)) i32
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

(defun main () i32
  (print "fib(30) = ")
  (println (fib 30))
  0)
~~~

Output:

~~~
fib(30) = 832040
~~~

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
