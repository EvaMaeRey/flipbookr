text_prep <- function(text = "This is my text.  Return it one sentence per page. Thanks",
                      sep = "\\. +|! +",
                      md_header = "",
                      sep_replace = "",
                      break_type = "---",
                      class = "inverse, middle, center"
){

  text %>%
    stringr::str_split(pattern = sep) %>%
    .[[1]] -> segments


  glue::glue(
    {break_type},
    "class: {class}",
    "{md_header} {segments}{sep_replace}",
    "",
    .sep = "\n")

}


text_reveal <- function(text,
                        sep, md_header = "#",
                        sep_replace = "",
                        break_type = "---",
                        class = "inverse, middle, center"){

  the_text <- text_prep(text = text,
                        sep = sep,
                        md_header = md_header,
                        sep_replace = sep_replace,
                        break_type = break_type,
                        class = class
  )

  paste(knitr::knit(text = the_text), collapse = "\n")

}

