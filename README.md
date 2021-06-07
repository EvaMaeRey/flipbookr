
<!-- README.md is generated from README.Rmd. Please edit that file -->

# The flipbookr package

<!-- badges: start -->
<!-- badges: end -->

“Flipbooks” present side-by-side, aligned, incremental code-output
evolution via automated code parsing and reconstruction. Like physical
flipbooks, they let the ‘reader’ watch a scene evolve at their own pace.
Flipbooks seek to reduce the guesswork involved between code and its
behavior by presenting substeps of a coding pipeline; the reader of a
flipbook observes the partial code that is used to create “A.1”, “A.2”,
“A.3” etc. all the way up to “B”.

Here’s the ‘minimal flipbook’ template that’s available with the
package:

<a href="https://evamaerey.github.io/flipbookr/minimal_flipbook.html" target="_blank">View flipbook in a new tab</a>

<img src="https://evamaerey.github.io/flipbookr/minimal_flipbook.gif" width="100%" />

The create a flipbook isn’t great because parsing and reconstruction of
code pipelines into substeps is automated!

<a href="https://evamaerey.github.io/flipbookr/minimal_flipbook_double_crochet.html" target="_blank">View <em>doublecrochet</em> flipbook new tab</a>

<img src="https://evamaerey.github.io/flipbookr/minimal_flipbook_double_crochet.gif" width="100%" />

## Installation

You can install the development version of flipbookr with devtools as
follows:

``` r
devtools::install_github("EvaMaeRey/flipbookr")
```

You will most likely use this package with the rmarkdown presentation
tool, Xaringan, which is available on CRAN:

``` r
install.packages("xaringan")
```

## Template

The package includes a template for building a flipbook that
demonstrates various flipbooking modalities; the template can also be
accessed from within RStudio (New File -&gt; RMarkdown -&gt; From
Template -&gt; A Minimal Flipbook)
[here](https://raw.githubusercontent.com/EvaMaeRey/flipbookr/master/inst/rmarkdown/templates/minimal-flipbook/skeleton/skeleton.Rmd).
Here is a preview:

<img src="https://github.com/EvaMaeRey/flipbooks/blob/master/flipbookr/skeleton.gif?raw=true" width="100%" />

The full flipbook, an html slideshow, that you can advance at your own
pace, can be viewed
[here](https://evamaerey.github.io/flipbooks/flipbookr/skeleton#1).

## Intuition about how it works

We believe in communicating with flipbooks, so we use the tool to
describe the functions that are at work within the package
[here](https://evamaerey.github.io/flipbooks/flipbookr/flipbookr_building_blocks#1)

## please check out the code of conduct for contributors.
