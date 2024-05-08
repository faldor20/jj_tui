# eio-process

[![CI Status](https://github.com/mbarbin/eio-process/workflows/ci/badge.svg)](https://github.com/mbarbin/eio-process/actions/workflows/ci.yml)
[![Coverage Status](https://coveralls.io/repos/github/mbarbin/eio-process/badge.svg?branch=main)](https://coveralls.io/github/mbarbin/eio-process?branch=main)

This is an experimental library to spawn external processes in
[Eio](https://github.com/ocaml-multicore/eio) with an api that resembles
[Async.Process](https://github.com/janestreet/async_unix).

This project re-uses some function and type names from the `Async_unix.Process`
interface. The implementation however is quite different, since the original
runs in the `Async` monad, whereas this lib targets `Eio`.

## Motivation

We find that this API offers convenient wrappers that we believe are a good fit
on top of the core functionality offered by `Eio.Process`.

## Usage

`Eio_process` is meant to be used directly as a top-level module, alongside
other `Eio` modules. We do not recommend shadowing `Eio.Process` with
`Eio_process` in user code at this time.

## Acknowledgements

We would like to express our gratitude to the `Eio` developers for their work on
the [eio](https://github.com/ocaml-multicore/eio) project, and for the original
module `Eio.Process` that this project extends. `Eio` is released under the
terms of an ISC License. Its copyright and permission notice are included at the
root of this project, in the file `LICENSE.eio`.

We also appreciate the work done by the async team at Jane Street and their
contribution to the open source community. We're thankful for the api exposed by
the `Async_unix.Process` module which we took inspiration from in this project.
`Async_unix` is released under the terms of an `MIT` License. Its copyright and
permission notice are included at the root of this project, in the file
`LICENSE.janestreet`.

## Code documentation

The code documentation of the latest release is built with `odoc` and published
to `GitHub` pages [here](https://mbarbin.github.io/eio-process).
