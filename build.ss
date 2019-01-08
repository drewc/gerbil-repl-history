#!/usr/bin/env gxi

(import :std/build-script
        :std/make)

(defbuild-script
  `((gsc: "repl-history" ,@(include-gambit-sharp))
    (ssi: "repl-history")))
