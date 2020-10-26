
###### write instant flipbook source

write_instant_flipbook_source <- function(rmd_path,
                                          title = "",
                                          subtitle = "",
                                          author = "",
                                          break_type = break_type,
                                          use_share_again = F,
                                          code_file_name,
                                          chunk_name, #mostly labels expanded chunks
                                          font_size = 100,
                                          title_page = F,
                                          ...
                                          ){

readLines(code_file_name) %>%
    paste(collapse = "\n") ->
    the_code


writeLines(text =
             paste0(
'---
title: "', title , '"
subtitle: "', subtitle , '"
author: "', author , '"
output:
  xaringan::moon_reader:
    seal: ', title_page, '
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


```{r the_chunk, include = F}
',the_code,'
```

`r chunk_reveal(chunk_name = "the_chunk", break_type = "', break_type, '", ...)`



```{css, eval = TRUE, echo = FALSE}
.remark-code{line-height: 1.5; font-size: ', font_size,'%}

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

embed_flipbook <- function(chunk_name,
                                   break_type = "auto",
                                   code_file_name = paste0(chunk_name, ".R"),
                                   rmd_path = paste0(chunk_name, ".Rmd"),
                                   title = str_replace(chunk_name, "_|\\.", " "),
                                   subtitle = "",
                                   author = "",
                                   use_share_again = F,
                                   url = paste0(chunk_name, ".html"),
                                   height = 360,
                                   font_size = 200,
                                   use_embed_xaringan = F,
                                   title_page = F,
                                   ...
                                   ){

  save.image("current_image.Rdata") # in case something is needed from it in instant fb

  knitr::knit_code$get(chunk_name) %>%
    paste(collapse = "\n") %>%
    writeLines(code_file_name)

  write_instant_flipbook_source(chunk_name = chunk_name,
                                break_type = break_type,
                                code_file_name = code_file_name,
                                rmd_path = rmd_path,
                                title = title,
                                subtitle = subtitle,
                                author = author,
                                use_share_again = use_share_again,
                                font_size = font_size,
                                title_page = title_page,
                                ...
                                )
  rmarkdown::render(rmd_path, quiet = T)

  if(use_embed_xaringan == T){

  xaringanExtra::embed_xaringan(url = url)

  } else {

  knitr::include_url(url = url, height = height)

  }

}





