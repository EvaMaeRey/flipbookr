# flipbookr

<!-- badges: start -->
<!-- badges: end -->

"Flipbooks" are tools that present side-by-side, aligned, incremental code-output evolution via automated code parsing and reconstruction.  Existing "dynamic documents" provide valuable insights about code behavior, by presenting code blocks together with their output.  Yet many substeps may be contained within a single pipeline of a code block.  If the reader of the dynamic document is familiar with many of the substeps, the reader won't have too much trouble inferring what must be happening in unfamiliar bits.  However, if many substeps are unfamiliar to the reader, linking code and behavior is a much trickier business --- how a block of code transforms imput "A" to output "B" may be murky for a newcomer. Flipbooks seek to reduce the guesswork involved between code and its behavior by presenting substeps of a coding workflow; the reader of a flipbook observes the partial code that is used to create "A.1", "A.2", "A.3" etc. all the way up to "B". The additional burden to create a flipbook (versus a traditional dynamic document) is not great because parsing and reconstruction of code pipelines into substeps is automated.  

## Installation

You can install the development version of flipbookr from [CRAN](https://CRAN.R-project.org) with:

``` r
devtools::install_github("EvaMaeRey/flipbookr")
```

## Example

In progress.  


