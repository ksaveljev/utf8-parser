# utf8-parser

Working through a [tutoria](https://ianthehenry.com/2015/1/17/decoding-utf-8/)
on how to build UTF-8 parser.

The parser itself has no significant value but working through the tutorial
itself has enough value in it. You read about Unicode, some of its specs and
caveats. You get to practice using Attoparsec.

In the end we get a parser which is able to parse UTF-8 but the error handling
is somewhat bad: our error messages do not give us any useful information.

#

Running:

    cabal sandbox init
    cabal install --dependencies-only
    cabal run
