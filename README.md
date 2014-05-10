# frontier

A lightweight ClojureScript library that presents pedestal-app like
approach to handling state and control.

The general idea is to be able to write apps functionally with a sane
way to handle composability and side effects.

I am extremely interested in maintaining a transaction boundary at the
edge of the app state. I want to always be able to have a
deterministic answer to the question what is the app state given a
certain list of inputs.

The paper ["Out of the Tar
Pit"](http://shaffner.us/cs/papers/tarpit.pdf) is my main reference
for this library but was also heavily inspired by pedestal and the
composability of React.

This lib is renderer agnostic.

There are two examples in the `examples/frontier_examples/components.cljs` directory.

## Usage

Still very much a work in progress.

## License

Copyright © 2014 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
