Encore
======

Encore is a little library to provide an interface to generate an
[Angstrom](https://github.com/inhabitedtype/angstrom.git)'s decoder and a
Condorcet's encoder from a shared description. The goal is specifically for
[ocaml-git](https://github.com/mirage/ocaml-git.git) and ensure isomorphism when
we decode and encode a Git object - and keep the same hash/identifier.

Examples
========

A good example can be found in `test/` directory. It provides a description of a
Git object and, by this way, make an Angstrom decoder and a Condorcet encoder.
Then, we test the Encore git repository itself to check integrity after a
serialization and a de-serialization.

Inspirations
============

This project is inspired to the [finale](https://github.com/takahisa/finale.git)
project which is focus on a pretty-printer at the end. Encore is close to
provide a low-level encoder like
[Faraday](https://github.com/inhabitedtype/faraday.git) than a generator of a
pretty-printer.
