
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

<img src="https://evamaerey.github.io/flipbookr/minimal_flipbook.gif" width="75%" style="display: block; margin: auto;" />

The create a flipbook isn’t hard because parsing and reconstruction of
code pipelines into substeps is automated!

flipbookr’s `chunk_reveal()` disassembles a single code chunk and
creates the “build” of multiple partial-code chunks on different slides
(the — is automatically generated for you too).

Check out the details on how to do this in this
<a href="https://evamaerey.github.io/flipbookr/minimal_flipbook_double_crochet.html" target="_blank"><em>doublecrocheted</em> version of the same flipbook</a>
(quotes the .Rmd source on some slides).

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

The package includes several templates for building a flipbook that
demonstrates various flipbooking modes.

The templates can be accessed from within RStudio. For example: New File
-&gt; RMarkdown -&gt; From Template -&gt; A Minimal Flipbook. The
templates are:

-   A Minimal Flipbook
-   Most Flipbookr Features,
    <a href="https://raw.githubusercontent.com/EvaMaeRey/flipbookr/master/inst/rmarkdown/templates/minimal-flipbook/skeleton/skeleton.Rmd" target="_blank">preview output</a>
-   A Python Flipbook

## How it works:

[Here’s](https://evamaerey.github.io/flipbooks/flipbookr/flipbookr_building_blocks#1)
a flipbook going through some of the internal flipbookr functions.
