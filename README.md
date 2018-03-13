Encore
======

[![Build Status](https://travis-ci.org/dinosaure/encore.svg?branch=master)](https://travis-ci.org/dinosure/encore)
![MirageOS](https://img.shields.io/badge/MirageOS-%F0%9F%90%AB-red.svg)

Encore is a little library to provide an interface to generate an
[Angstrom](https://github.com/inhabitedtype/angstrom.git)'s decoder and a
Condorcet's encoder from a shared description. The goal is specifically for
[ocaml-git](https://github.com/mirage/ocaml-git.git) to ensure isomorphism when
we decode and encode a Git object - and keep the same hash/identifier.

Examples
========

A good example can be found in `test/` directory. It provides a description of a
Git object and, by this way, make an Angstrom decoder and a Condorcet encoder.
Then, we test the Encore git repository itself to check integrity after a
serialization and a de-serialization.

Benchmark
=========

Encore integrates a little overhead when you compare generated decoder/encoder
with an decoder and a decoder generated by hand. We integrate a benchmark
which compares a specific version of `ocaml-git` (encore branch) and
decoder/encoder produced by Encore. You can run this benchmark locally with
`jbuilder build @runbench` but first you need to pin `ocaml-git` on:

```
$ opam pin add git https://github.com/dinosaure/ocaml-git.git#encore
$ opam pin add git-http https://github.com/dinosaure/ocaml-git.git#encore
$ opam pin add git-unix https://github.com/dinosaure/ocaml-git.git#encore
```

Then, on my computer (Thinkpad X1 Carbon - Inteli i7-7500U CPU @ 2.70 Ghz - 2.90
Ghz), I get this result:

```
┌────────┬──────────┬─────────┬──────────┬──────────┬────────────┐
│ Name   │ Time/Run │ mWd/Run │ mjWd/Run │ Prom/Run │ Percentage │
├────────┼──────────┼─────────┼──────────┼──────────┼────────────┤
│ encore │  37.24ms │  3.45Mw │ 194.32kw │  18.09kw │    100.00% │
│ git    │  32.84ms │  3.52Mw │ 229.67kw │  13.92kw │     88.16% │
└────────┴──────────┴─────────┴──────────┴──────────┴────────────┘
```

So, we can observe a little overhead but guarantees provided by Encore are more
interesting than a faster decoder/encoder.

Some notes about Condorcet
==========================

Condorcet is a little encoder which takes care about the memory consumption when
you serialize an OCaml value with a description. We use a bounded bigarray and
when it's full, we explicitly ask to the user to flush it.

Condorcet was built on a CPS mind like Angstrom and uses only pure functional
data structures. This is a big difference from
[Faraday](https://github.com/inhabitedtype/faraday.git). So, obviously,
Condorcet is slower than Faraday (3 times), however, we can not use Faraday in
this context, precisely about _alteration_.

In fact, when Condorcet fails, we raise an exception to short-cut to the other
branch. With a mutable structure, it's little bit hard to rollback to the old
state of encoder and retry the other branch. With Condorcet, we don't need to
trick to rollback because, at any step we make a new pure state.

Inspirations
============

This project is inspired by the [finale](https://github.com/takahisa/finale.git)
project which is focused on a pretty-printer at the end. Encore is close to
provide a low-level encoder like
[Faraday](https://github.com/inhabitedtype/faraday.git) than a generator of a
pretty-printer.
