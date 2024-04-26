## v3.2.1 (2024-04-26)

* move lwt tests to shared-memory-ring-lwt (@hannesm)

## v3.2.0 (2024-04-24)

* revive example client (#41 @adatario)
* drop mirage-profile and ppx_cstruct dependency (#43 @hannesm #41 @adatario)
* shared-memory-ring-lwt: depend on shared-memory-ring =version

## v3.1.1 (2022-04-13)

* Remove the build directive on dune dependency (#39 @CraigFE)
* Fix compilaion, use OCaml 4.08 as lower bound, cstruct 6.0.0 compatibility (#40 @hannesm)

## v3.1.0 (2019-01-12)

* Port build from jbuilder to Dune (@avsm).
* Update opam metadata to the 2.0 format (@avsm). 
* Removed deprecated use of `Lwt_sequence` and depend
  on the equivalent `lwt-dllist` package instead (@avsm)
* Use `noalloc` attribute in external declarations (@avsm)
* Fix ocamldoc formatting with odoc (@avsm)
* Travis: test up to OCaml 4.07 (@avsm)

## 3.0.1 (2018-07-09)

* Use modern `ppx_cstruct` to build.

## 3.0.0 (2017-11-05)

* Update to use `bytes` in the `read` and `write`, to work with OCaml 4.06 and
  `-safe-string`

## 2.0.1 (2017-06-07)
* Don't force clients to link against `cstruct.ppx`: this is the support code
  for the PPX rewriter itself, and brings in the `compiler-libs` package etc.

## 2.0.0 (2017-05-28)
* Refactor the ocamlfind packages and opam packages into
  - shared-memory-ring: all the portable code including the -xen definitions
  - shared-memory-ring-lwt: the old shared-memory-ring.lwt

## 1.3.0 (2015-03-19)
* Use centralised Travis scripts and modernise OPAM file.
* Add Windows support

## 1.2.0 (2016-03-14)
* opam: ounit is a test dependency
* add LICENSE file
* remove dependency on cstruct.syntax: we use cstruct.ppx instead
  Note: this means we need OCaml 4.02
* remove dependency on lwt.syntax

## 1.1.1 (2015-01-09)
* add profiling/tracing support
* add a "Front.wait_for_free" function to wait for n free slots
* add opam file

## 1.1.0 (2014-07-16)
* [xen]: add a prepare/commit interface to control when xenstore/console data is consumed
* [xen]: add read and write to complement unsafe_read and unsafe_write

## 1.0.0 (2013-12-07):
* Standardize indentation by running through ocp-indent.
* Improve ocamldoc for `Ring` module.

## 0.4.3 (2013-10-05):
* Add Travis CI scripts.
* Fix ARMv5 compilation.

0.4.2 (2013-10-05):
* Properly initialise the shared event counters. This bug prevented the
  backend from notifying the frontend, causing deadlock.

0.4.1 (2013-07-25):
* Fix a ring corruption issue due to multi-byte stores and loads

0.4.0 (2013-04-30):
* Fix a ring corruption issue which could lead to dropped network packets
  or stalled disks
* Change ring API to improve debug messages

0.3.1 (2013-03-15):
* Fix unsigned comparisons in console/xenstore ring handling

0.3.0 (2013-02-22):
* Add support for ARM: each architecture requires support for memory barrier primitives
* For the xenstore-style rings, apply the memory barriers in the same way as the original xen implementation

0.2.0 (2012-02-08):
* Add `Lwt_ring` write and push functions that separate updates and notification, to support fragment-based protocols.
* Improve diagnostics support.

0.1.0 (2012-12-20):
* Initial public release.
* Update to new cstruct 0.6.0 API
* [xen] support for suspend and resume
* [xen] subpackages for console, xenstore rings
* [xen] fix data-loss bug in console, xenstore rings where a write
  was silently truncated when the ring was full
* Everything is pure OCaml except assembly for memory barriers and
  some unit tests
