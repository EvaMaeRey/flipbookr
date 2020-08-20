
text_prep <- function(text = "This is my text.   Return it one sentence per page.   Thanks",
                      sep = "   ",
                      md_header = "",
                      sep_replace = "",
                      slide_break = "---",
                      class = "class: inverse, middle, center"
){

  text %>%
    stringr::str_split(pattern = sep) %>%
    .[[1]] -> segments


  glue::glue(
    {slide_break},
    {class},
    "{md_prefix} {segments}{sep_replace}",
    "",
    .sep = "\n")

}


#' Title
#'
#' @param text a character string to be split and delivered piece-wise to a slide
#' @param sep a character string to delimit the split of the input text
#' @param md_prefix a character string prefix to each markdown element, defaults to "#"
#' @param sep_replace a character string that will replace the delimitor, defaults to empty string ""
#' @param slide_break a character string containing slide break characters, defaults to "---" for xaringan slideshows
#' @param class a character string in which you can set the class, defaults to "class: inverse, middle, center"
#'
#' @return
#' @export
#'
#' @examples
#' text_reveal("Hello world", sep = " ")
text_reveal <- function(text,
                        sep = "   ",
                        md_prefix = "#",
                        sep_replace = "",
                        slide_break = "---",
                        class = "class: inverse, middle, center"){

  the_text <- text_prep(text = text,
                        sep = sep,
                        md_prefix = md_prefix,
                        sep_replace = sep_replace,
                        slide_break = slide_break,
                        class = class
  )

  paste(knitr::knit(text = the_text), collapse = "\n")

}

