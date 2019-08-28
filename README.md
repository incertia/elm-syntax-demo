# elm-syntax-demo

This is a demo of an interactive syntax highlighting editing window for the
Haskell language, writtein in Elm, mainly for the purposes of learning Elm but
also so that the foundations of this demo can be turned into a syntax
highlighting framework.

The Haskell syntax parser currently is unable to parse anything context
sensitive, as it is a line by line parser as it is currently designed. This is
to get line numbering to work as expected without having to make exceptions for
what to do with multiline facilities. Adding in context can also let us parse
the identifier differently in a type signature as well as only highlight things
that are getting a definition (e.g. `main = print 0` would highlight `main` but
not `print`), but that is too much effort.

The main ideas towards getting this to work as expected are as follows:

1. The `textarea` has transparent foreground color and transparent background,
   but is placed on top of the backdrop.
2. The backdrop contains an idendical but marked up copy of the `textarea` and
   the transparency allows the text to be shown and it creates the illusion that
   the `textarea` is highlighted.
3. Scrolling is applied to the backdrop from values from the `textarea` to sync
   the two together. This is done via `transform="translate(-,-)"`.

The actual code to mark up a `Source` is not very complicated. Once a `Source`
representation is obtained it has a mostly 1-1 correspondence with the `div`s
that appear in the DOM. The language parser is also probably not idiomatic Elm
but coming from a Haskell background, it's what makes the most sense for me to
write at this current point in time.
