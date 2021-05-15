
<!-- README.md is generated from README.Rmd. Please edit that file -->

# The flipbookr package

<!-- badges: start -->
<!-- badges: end -->

“Flipbooks” are tools that present side-by-side, aligned, incremental  
code-output evolution via automated code parsing and reconstruction.
Like traditional flipbooks, they let the reader watch a scene evolve at
their own pace. Existing “dynamic documents” provide valuable insights
about code behavior, by presenting code blocks together with their
output. Yet many substeps may be contained within a single pipeline of a
code block. If the reader of the dynamic document is familiar with many
of the substeps, the reader won’t have too much trouble inferring what
must be happening in unfamiliar bits. However, if many substeps are
unfamiliar to the reader, linking code and behavior is a much trickier
business — how a block of code transforms input “A” to output “B” may be
murky for a newcomer. Flipbooks seek to reduce the guesswork involved
between code and its behavior by presenting substeps of a coding
workflow; the reader of a flipbook observes the partial code that is
used to create “A.1”, “A.2”, “A.3” etc. all the way up to “B”. The
additional burden to create a flipbook (versus a traditional dynamic
document) is not great because parsing and reconstruction of code
pipelines into substeps is automated.

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
