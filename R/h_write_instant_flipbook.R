
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
      ratio: 16:9
      highlightStyle: github
      highlightLines: true
      countIncrementalSlides: false
---



```{r, include = F}
options(knitr.duplicate.label = "allow")
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

code.r.hljs.remark-code{
  position: relative;
  overflow-x: hidden;
}


code.r.hljs.remark-code:hover{
  overflow-x:visible;
  width: 500px;
  border-style: solid;
}
```
')

, con = rmd_path)
}



# to be used in build instant flipbook
#' embed_flipbook
#'
#' @param chunk_name a character string referring named chunk containing code to 'flipbook'
#' @param break_type "auto" is default finding appropriate breakpoints, "user" can be used with the special comment message #BREAK within the source code chunk, "non_seq" can be used for non sequential display of code with special comment messages #BREAK2 (will show in second frame) and #BREAK3 (will show in third frame), an integer input can be given too, to simply display the source code chunk multiple times which is appropriate for observing multiple realizations of sampling, "rotate" allows cycling through different lines of code, the comment #ROTATE is used for lines to by cycled through
#' @param code_file_name a .R file path where chunk's code will be saved
#' @param rmd_path an .Rmd path were source of mini flipbook will be saved
#' @param title a character string if a title is desired for the embedded flipbook, defaults to modified chunk name
#' @param subtitle a character string for the embedded flipbook's subtitle, defaults to ""
#' @param author a character string for the embedded flipbook's author info, defaults to ""
#' @param url path to .html rendered mini flipbook
#' @param height numeric size of iframe, defaults to 325
#' @param font_size numeric to adjust the size of code in embedded flipbooks
#' @param title_page logical indicating whether to include a title page for the mini flipbook, defaults to FALSE
#' @param ... inherits from chunk_reveal()
#'
#' @return
#' @export
embed_flipbook <- function(chunk_name,
                           break_type = "auto",
                           code_file_name = paste0("embedded_flipbooks/", chunk_name, ".R"),
                           rmd_path = paste0("embedded_flipbooks/", chunk_name, "_embed.Rmd"),
                           title = stringr::str_replace_all(chunk_name, "_|\\.", " "),
                           subtitle = "",
                           author = "",
                           # use_share_again = F,
                           url = paste0("embedded_flipbooks/", chunk_name, "_embed.html"),
                           height = 325,
                           font_size = 120,
                           # use_embed_xaringan = F,
                           title_page = F,
                                   ...
                                   ){

  if(!dir.exists("embedded_flipbooks")){dir.create("embedded_flipbooks")}
  save.image("embedded_flipbooks/current_image.Rdata") # in case something is needed from it in instant fb

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
                                # use_share_again = use_share_again,
                                font_size = font_size,
                                title_page = title_page,
                                ...
                                )

  rmarkdown::render(rmd_path, quiet = T)

  # xaringanExtra is development package, so this is not allowed for now.
  # if(use_embed_xaringan == T){
  #
  # xaringanExtra::embed_xaringan(url = url)
  #
  # } else {

  knitr::include_url(url = url, height = height)

  # }

}



###### write instant flipbook source

write_instant_text_flipbook_source <- function(text,
                                               rmd_path,
                                          title = "",
                                          subtitle = "",
                                          author = "",
                                          use_share_again = F,
                                          font_size = 100,
                                          title_page = F,
                                          ...){



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
      ratio: 16:9
      highlightStyle: github
      highlightLines: true
      countIncrementalSlides: false
---



```{r, include = F}
options(knitr.duplicate.label = "allow")
knitr::opts_chunk$set(fig.width = 6, message = FALSE, warning = FALSE, comment = "", cache = F)
library(flipbookr)
library(tidyverse)
# load("current_image.Rdata")
```


```{r, echo = F, eval = ', use_share_again,' , message = F, warning = F}
xaringanExtra::use_share_again()
```




`r text_reveal(text = "', text, '", ...)`



```{css, eval = TRUE, echo = FALSE}
.remark-code{line-height: 1.5; font-size: ', font_size,'%}

@media print {
  .has-continuation {
    display: block;
  }
}

code.r.hljs.remark-code{
  position: relative;
  overflow-x: hidden;
}


code.r.hljs.remark-code:hover{
  overflow-x:visible;
  width: 500px;
  border-style: solid;
}
```
')

             , con = rmd_path)
}




text_explode <- function(text, sep = "   ",
                                title = "Brought to you by Essay Exploder",
                                title_snake = stringr::str_replace(tolower(title), " ", ""),
                                rmd_path = paste0("exploded_texts/", title_snake, ".Rmd"),
                                subtitle = "",
                                author = "",
                                use_share_again = F,
                                url = paste0("exploded_texts/", title_snake, ".html"),
                                height = 325,
                                font_size = 120,
                                title_page = T,
                                ...
){

  # save.image("embedded_flipbooks/current_image.Rdata") # in case something is needed from it in instant fb

  if(!dir.exists("exploded_texts")){dir.create("exploded_texts")}


  write_instant_text_flipbook_source(text = text,
                                     rmd_path = rmd_path,
                                     title = title,
                                     subtitle = subtitle,
                                     author = author,
                                     use_share_again = use_share_again,
                                     font_size = font_size,
                                     title_page = title_page,
                                     ...)

  rmarkdown::render(rmd_path, quiet = T)

}



embed_text_flipbook <- function(text,
                                title,
                                title_snake = stringr::str_replace(tolower(title), " ", ""),
                           rmd_path = paste0("embedded_flipbooks/", title_snake, "_text_embed.Rmd"),
                           subtitle = "",
                           author = "",
                           use_share_again = F,
                           url = paste0("embedded_flipbooks/", title_snake, "_text_embed.html"),
                           height = 325,
                           font_size = 120,
                           use_embed_xaringan = F,
                           title_page = F,
                           ...
){

  save.image("embedded_flipbooks/current_image.Rdata") # in case something is needed from it in instant fb


  write_instant_text_flipbook_source(text = text,
                                                 rmd_path = rmd_path,
                                                 title = title,
                                                 subtitle = subtitle,
                                                 author = author,
                                                 use_share_again = use_share_again,
                                                 font_size = font_size,
                                                 title_page = title_page,
                                                 ...)

  rmarkdown::render(rmd_path, quiet = T)

  # development package so not allowed
  # if(use_embed_xaringan == T){
  #
  #   xaringanExtra::embed_xaringan(url = url)
  #
  # } else {

    knitr::include_url(url = url, height = height)

  # }

}
