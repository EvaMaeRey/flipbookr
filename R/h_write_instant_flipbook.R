write_instant_flipbook <- function(file){

writeLines(text =
'---
title: "Instant Flipbook"
subtitle: "With flipbookr and xaringan"
author: "Gina Reynolds, December 2019"
output:
  xaringan::moon_reader:
    seal: FALSE
    lib_dir: libs
    css: [default, hygge, ninjutsu]
    nature:
      ratio: 16:10
      highlightStyle: github
      highlightLines: true
      countIncrementalSlides: false
---



```{r, include = F}
# This is the recommended set up for flipbooks
# you might think about setting cache to TRUE as you gain practice --- building flipbooks from scratch can be time consuming
knitr::opts_chunk$set(fig.width = 6, message = FALSE, warning = FALSE, comment = "", cache = F)
library(flipbookr)
library(tidyverse)
```


```{r prep code seq, include = F}
readLines("temp.R") %>%
  paste(collapse = "\n") %>%
  code_parse() %>%
  parsed_return_partial_code_sequence() ->
the_code_seq
```


`r chunk_reveal(code_seq = the_code_seq)`



```{css, eval = TRUE, echo = FALSE}
.remark-code{line-height: 1.5; font-size: 80%}

@media print {
  .has-continuation {
    display: block;
  }
}
```
', con = file)
}

