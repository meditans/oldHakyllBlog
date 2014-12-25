---
title: Hakyll Haskell Highlighting Setup
author: Carlo Nucera
---

Now, some haskell stuff:

~~~~{.haskell}
import Data.List

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
~~~~

But there isn't any syntax highlighting!!!

Now, It is. I basically added [this snippet][haskellCss] in my
css/default.css file. Now, I'd wonder what should I do for a proper
*solarized* highlighting. Basically it should suffice to change the
colors inside of the css section and maybe then publish it as a gist
on github.

[haskellCss]: https://github.com/jaspervdj/hakyll/blob/master/web/css/syntax.css
