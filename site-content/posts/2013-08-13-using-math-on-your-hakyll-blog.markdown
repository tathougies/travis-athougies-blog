---
title: Using LaTeX Math on your Hakyll blog
author: Travis Athougies
tags: "hakyll, haskell, web"
published: true
---

[Hakyll](http://jaspervdj.be/hakyll) is a really powerful static website tool written in
Haskell. Basically, it takes a bunch of text files in Markdown and puts them together into an
awesome static website (like this one!). Out of the box, Hakyll already does a lot, but one thing it
doesn't do is math formulae.

Luckily this isn't too hard to set up. Hakyll uses the wonderful
[Pandoc](http://johnmacfarlane.net/pandoc/) package to generate HTML, and Pandoc supports embedding
math in HTML. According to [this page](), all we need to do is enable the `tex_math_dollars`
option. Just for extra swag, we'll enable the `tex_math_double_backslash` and `latex_macros`
options. This will let Pandoc understand latex block math and to expand latex macros that we define
in our Markdown files!

To do this, we're going to modify the Pandoc compiler shipped with Hakyll. First we need to import
some modules

``` haskell
import qualified Data.Set as S
import           Text.Pandoc.Options
```

Now we're going to define our new compiler. By default, Hakyll uses the `pandocCompiler` function to
compile Markdown files into HTML. This function uses the default Hakyll options to generate
HTML. There's also the `pandocCompilerWith` function, which does take options. Its type signature is

``` haskell
pandocCompilerWith :: ReaderOptions -> WriterOptions -> Compiler (Item String)
```

Since we're only going to want to change how Pandoc outputs HTML, we're going to use the default
reader options in `defaultHakyllReaderOptions`. If we take a look at the
[`WriterOptions` data type](http://hackage.haskell.org/packages/archive/pandoc/1.10.0.4/doc/html/Text-Pandoc-Options.html#t:WriterOptions),
we find two fields that we might be interested in: `writerHTMLMathMethod` and `writerExtensions`. The
`writerHTMLMathMethod` field tells Pandoc how we want it to output the math in HTML. I found the
easiest way to be using [Mathjax](http://mathjax.org). Looking at the `HTMLMathMethod` type, it
looks like we'll need to set this field to `MathJax ""`. Now, to enable the three extensions in our
compiler, we'll need to add `Ext_tex_math_dollars`, `Ext_tex_math_double_backslash`, and
`Ext_latex_macros` into the `writerExtensions` set. We can do this by folding with the
`Data.Set.insert` function.

Let's define our new Pandoc math compiler!

``` haskell
pandocMathCompiler =
    let mathExtensions = [Ext_tex_math_dollars, Ext_tex_math_double_backslash,
                          Ext_latex_macros]
        defaultExtensions = writerExtensions defaultHakyllWriterOptions
        newExtensions = foldr S.insert defaultExtensions mathExtensions
        writerOptions = defaultHakyllWriterOptions {
                          writerExtensions = newExtensions,
                          writerHTMLMathMethod = MathJax ""
                        }
    in pandocCompilerWith defaultHakyllReaderOptions writerOptions
```

Most of this should be self-explanatory, but essentially, we modify the default Hakyll Pandoc
options to use the new math output method and to use the appropriate math extensions.

Now, all we have to do is replace all instances of `pandocCompiler` with `pandocMathCompiler` in our
site's `main` function, and we're almost good to go! I say almost, because you will probably need to
add the following line into your `templates/default.html` (or whatever template contains your HTML
`<head>` element):

``` html
<script type="text/javascript"
        src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"></script>
```

Now, try adding math to one of your pages and see if it works. A quick word of warning though: to do
the LaTeX `\[` and `\]` you need to double up on slashes, and you need to double the slashes before
each command.

As a quick proof, here's what it looks like rendering one definition of the natural log.

\\[ \\ln x = \\int_{-\\infty}^x \\frac 1 y \\, dy . \\]

This was generated from the code:

``` markdown
\\[ \\ln x = \\int_{-\\infty}^x \\frac 1 y \\, dy . \\]
```

Great! You're all set to use math on your shiny new Hakyll blog! Happy blogging! :D