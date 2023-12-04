# Advent of Code 2023
---

I love Advent of Code.
It's a chance to have some fun programming.
[Give it a try!](https://adventofcode.com/)

All the code should just work. I use NixOS:

    $ nix-shell

but you don't have to:

    $ cabal run day01 -v0 < i/01 > o/01

where `i/` and `o/` hold the input and output for each problem.

Note that I am using a [custom prelude](
https://github.com/instinctive/edu-advent-2023/tree/main/prelude)
to keep the `import` statements under control.
