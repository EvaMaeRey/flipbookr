
###### write instant flipbook source

write_instant_flipbook_source <- function(rmd_path, use_share_again = F, title = "", code_file_name = "temp.R"){

writeLines(text =
             paste0(
'---
title: "', title , '"
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
knitr::opts_chunk$set(fig.width = 6, message = FALSE, warning = FALSE, comment = "", cache = F)
library(flipbookr)
library(tidyverse)
load("current_image.Rdata")
```


```{r, echo = F, eval = ', use_share_again,' , message = F, warning = F}
xaringanExtra::use_share_again()
```

```{r, include = F}
readLines("', code_file_name ,'") %>%
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
')

, con = rmd_path)
}

# to be used in build instant flipbook

build_instant_flipbook <- function(chunk_name,
                                   rmd_path,
                                   use_share_again = F,
                                   title = "",
                                   code_file_name = "temp.R"){

  knitr::knit_code$get(chunk_name) %>%
    paste(collapse = "\n") %>%
    writeLines(code_file_name)

  save.image("current_image.Rdata") # in case something is needed from
  write_instant_flipbook_source(rmd_path = rmd_path,
                                use_share_again = use_share_again,
                                title = title,
                                code_file_name = code_file_name)
  rmarkdown::render(file, quiet = T)

}

