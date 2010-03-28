---
layout: post
title: Updated backtrace patch
---

I've updated my
[backtrace] (http://skydeck.com/blog/programming/stack-traces-in-ocaml)
[patch] (http://skydeck.com/blog/programming/more-stack-traces-in-ocaml)
to work with OCaml 3.11.x as well as 3.10.x. The patch provides

 * access to backtraces from within a program (this is already
   provided in stock 3.11.x)

 * backtraces for dynamically-loaded bytecode

 * backtraces in the (bytecode) toplevel

In addition there are a few improvements since the last version:

 * debugging events are allocated outside the heap, so memory use
   should be better with forking (on Linux at least, the data is
   shared on copy-on-write pages but the first GC causes the pages be
   copied)

 * fixed a bug that could cause spurious "unknown location" lines in
   the backtrace

 * a script to apply the patch (instead of the previous multi-step
   manual process)

See [ocaml-backtrace-patch] (http://github.com/jaked/ocaml-backtrace-patch) on Github or
[download the tarball] (http://github.com/downloads/jaked/ocaml-backtrace-patch/ocaml-backtrace-patch-0.5.tar.gz).
