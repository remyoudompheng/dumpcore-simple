dumpcore-simple
===

[![Build Status](https://travis-ci.org/githubuser/dumpcore-simple.png)](https://travis-ci.org/githubuser/dumpcore-simple)

See https://githubuser.github.io/dumpcore-simple/index.html for project description.

~~~
stack build --test --exec "$(stack path --local-install-root)/bin/dumpcore-simple-example" --exec "$(stack path --local-bin)/pandoc -f markdown+lhs -i app/example.lhs -t html -o index.html --filter pandoc-include --mathjax" --file-watch
~~~
