# Interim

[![Build Status](https://travis-ci.org/eudoxia0/interim.svg?branch=master)](https://travis-ci.org/eudoxia0/interim)

**Interim** is a statically-typed, low-level dialect of Lisp featuring
compile-time, GC-free memory management.

Interim is a technology demonstrator for compile-time memory management
using [regions][region-cyclone]. As of 2018, the only major language with
compile-time memory management is [Rust][rust], which is notoriously tough to
learn. Interim was built to prove that sound, GC-free, compile-time memory
management can be both simple to implement and easy to learn and use.

## Overview of Regions

Memory safety is achieved by preventing three classes of errors:

- `NULL` pointers
- Double `free()`
- Use after `free()` (dangling pointers)

Preventing `NULL` pointers is easy: we use an option type (called `Maybe` in
Haskell). In the case of Interim, we have a special pointer type called
`nullable` which represents a pointer which is potentially `NULL`. The `case`
construct can be used to extract a never-`NULL` pointer in a safe way.

Preventing double `free()` and use after `free()` errors is harder, and this is
where regions come in.

Consider this code:

~~~lisp
(letregion rho
  (let ((p (allocate rho 10)))
    (letregion rho'
      (let ((p' (allocate rho' 12)))
        nil))))
~~~

What we're doing here is:

1. Defining a region `rho`. Regions are both lexical objects and run-time values.
2. Defining a variable `p`, whose initial value is the result of allocating the
   number 10 in the region `rho`.

   An `allocate` call takes a value of type `T` and a region indentifier,
   allocates enough memory in the region to hold that value, and returns a
   pointer to it. The result pointer has the type `(nullable T R)`, where `T` is
   the type of the value we're allocating and `R` is the region identifier.

3. Defining a new region `rho'`.
4. Defining a new variable `p'`, whose initial value is `(allocate rho' 12)`,
   that is, the result of allocating the value `12` in the region `rho'`.
5. Finally, return `nil`.

Now notice what happens if we try to store `p'` in `p`:

~~~lisp
(letregion rho
  (let ((p (allocate rho 10)))
    (letregion rho'
      (let ((p' (allocate rho' 12)))
        (<- p p')))))
~~~

The compiler will fail with

~~~
Cannot assign to variable 'p': the type of the variable is (nullable i32 rho),
while the type of the expression is (nullable i32 rho')
~~~

This is the key to preventing dangling pointer errors: _pointers are tagged with
the region they belong to_. A pointer cannot escape its lifetime, to a higher-up
region or to a global variable, because the types won't match.

Double `free()` errors are prevented through a straightforward feature of
regions: there is no way to do a manual `free()`, and everything in a region is
freed automatically when exiting the region's body.

## Examples

For more examples, look in the `examples/` directory.

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

### Regions

~~~lisp
(defun main () i32
  (letregion rho
    (let ((p (allocate rho 10)))
      (case p
        ((not-null p')
         (print "Allocated successfully! Value: ")
         (println (load p'))
         0)
        (null
         (println "Out of memory!")
         -1)))))
~~~

Output:

~~~
Allocated successfully! Value: 10
~~~

## Building

You need [MLton][mlton], git and make to build Interim. On Ubuntu:

~~~bash
$ sudo apt-get install mlton git make
~~~

Then:

~~~bash
$ make interim
~~~

After building, try

~~~bash
$ make examples
~~~

### Dependencies

The only dependency is [Parsimony][parsimony], a parser combinator library.

## Design

Being a technology demonstrator, Interim lacks many features of real languages:
modules, macros, higher-order functions, and higher-order types are not
implemented since the focus is on region-based memory management.

As the name implies, Interim is a stepping stone or proof of concept for a
larger, more sophisticated language.

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
[mlton]: http://mlton.org/
[parsimony]: https://github.com/eudoxia0/parsimony

[region-cyclone]: https://www.cs.umd.edu/projects/cyclone/papers/cyclone-regions.pdf
[safe-mem]: http://www.cs.umd.edu/projects/PL/cyclone/scp.pdf
[explicit]: http://titanium.cs.berkeley.edu/papers/gay-thesis.pdf
