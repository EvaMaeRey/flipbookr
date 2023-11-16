
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

[View flipbook in a new
tab](https://evamaerey.github.io/flipbookr/minimal_flipbook.html)

<img src="https://evamaerey.github.io/flipbookr/minimal_flipbook.gif" width="75%" style="display: block; margin: auto;" />

The create a flipbook isn’t hard because parsing and reconstruction of
code pipelines into substeps is automated\!

flipbookr’s `chunk_reveal()` disassembles a single code chunk and
creates the “build” of multiple partial-code chunks on different slides
(the — is automatically generated for you too).

Check out the details on how to do this in this [*doublecrocheted*
version of the same
flipbook](https://evamaerey.github.io/flipbookr/minimal_flipbook_double_crochet.html)
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
-\> RMarkdown -\> From Template -\> A Minimal Flipbook. The templates
are:

  - A Minimal Flipbook
  - Most Flipbookr Features, [preview
    output](https://raw.githubusercontent.com/EvaMaeRey/flipbookr/master/inst/rmarkdown/templates/minimal-flipbook/skeleton/skeleton.Rmd)
  - A Python Flipbook

## How it works:

[Here’s](https://evamaerey.github.io/flipbooks/flipbookr/flipbookr_building_blocks#1)
a flipbook going through some of the internal flipbookr functions.

``` r
library(tidyverse)
#> ── Attaching core tidyverse packages ─────────────────── tidyverse 2.0.0.9000 ──
#> ✔ dplyr     1.1.0     ✔ readr     2.1.4
#> ✔ forcats   1.0.0     ✔ stringr   1.5.0
#> ✔ ggplot2   3.4.4     ✔ tibble    3.2.1
#> ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
#> ✔ purrr     1.0.1     
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
Titanic %>% 
  data.frame()
#>    Class    Sex   Age Survived Freq
#> 1    1st   Male Child       No    0
#> 2    2nd   Male Child       No    0
#> 3    3rd   Male Child       No   35
#> 4   Crew   Male Child       No    0
#> 5    1st Female Child       No    0
#> 6    2nd Female Child       No    0
#> 7    3rd Female Child       No   17
#> 8   Crew Female Child       No    0
#> 9    1st   Male Adult       No  118
#> 10   2nd   Male Adult       No  154
#> 11   3rd   Male Adult       No  387
#> 12  Crew   Male Adult       No  670
#> 13   1st Female Adult       No    4
#> 14   2nd Female Adult       No   13
#> 15   3rd Female Adult       No   89
#> 16  Crew Female Adult       No    3
#> 17   1st   Male Child      Yes    5
#> 18   2nd   Male Child      Yes   11
#> 19   3rd   Male Child      Yes   13
#> 20  Crew   Male Child      Yes    0
#> 21   1st Female Child      Yes    1
#> 22   2nd Female Child      Yes   13
#> 23   3rd Female Child      Yes   14
#> 24  Crew Female Child      Yes    0
#> 25   1st   Male Adult      Yes   57
#> 26   2nd   Male Adult      Yes   14
#> 27   3rd   Male Adult      Yes   75
#> 28  Crew   Male Adult      Yes  192
#> 29   1st Female Adult      Yes  140
#> 30   2nd Female Adult      Yes   80
#> 31   3rd Female Adult      Yes   76
#> 32  Crew Female Adult      Yes   20
```

## Coming soon

``` r
chunk_reveal_live(chunk_name = "sample_chunk")
```
