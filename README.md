# design-tools

## Dedication

> Automation may be a good thing, but don’t forget that it began with
> Frankenstein.
>
> -Anonymous

## Overview

`design-tools` is a Pandoc filter for building my new book [Design and
Interpretation of Haskell Programs][dihp].

[dihp]: https://www.patreon.com/designandinterpretation


### Block Transformations

`[file:defn](Snip)` will inline the definition of `defn` in `file` as a Haskell
code fence. For example, given `data Haskell = Haskell` in a file `Foo.hs`, the
command `[Foo.hs:Haskell](Snip)` will produce:

```haskell
data Haskell = Haskell
```

---

```markdown
Exercise

: blah blah blah
```

expands into `\begin{exercise}blah blah blah\end{exercise}`. You can use other
block elements in the `:` bit so long as they are indented by 4.

### Inline Transformations

`[txt](Ann)` will produce `\ann{txt}`, which is useful for drawing code
annotation circles.

