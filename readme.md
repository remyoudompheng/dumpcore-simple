dumpcore-simple
===

A family of GHC plugins dedicated to export compiler intermediate
representations in a human-readable way.

Usage instructions
------------------

Add the following to your stack.yaml

    extra-deps:
    - git: https://github.com/remyoudompheng/dumpcore-simple
      commit: COMMIT_SHA1

    ghc-options:
        "$locals":
                -fplugin=Compiler.Plugin.Trace
                -fplugin=Compiler.Plugin.Dumpcore
                -fplugin=Compiler.Plugin.DumpSTG

Then run `stack install dumpcore-simple' and build normally.
