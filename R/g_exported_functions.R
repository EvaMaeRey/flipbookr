

knit_text_and_collapse <- function(text){

  paste(knitr::knit(text = text), collapse = "\n")

}

######## The exported functions ############

#' Function takes code from referenced code chunk and returns partial code
#' sequence to series of code chunks separated by slide breaks.
#' Upon compiling you get step-by-step code walk-through.
#'
#' @param chunk_name a character string referring to the name of the source chunk for the flipbooking
#' @param break_type "auto" is default finding appropriate breakpoints, "user" can be used with the special comment message #BREAK within the source code chunk, "non_seq" can be used for non sequential display of code with special comment messages #BREAK2 (will show in second frame) and #BREAK3 (will show in third frame), an integer input can be given too, to simply display the source code chunk multiple times which is appropriate for observing multiple realizations of sampling, "rotate" allows cycling through different lines of code, the comment #ROTATE is used for lines to by cycled through
#' @param left_assign a logical, default is FALSE, if TRUE will print the object created in the upper left hand corner of the source code chunk at the end of each partial reveal
#' @param left_assign_add a character string containing function for table formatting in output, for left assign case only
#' @param code_seq a list of code as character strings, the list will automatically be created based on the previous three arguments or the user can input code manually
#' @param num_breaks an integer, automatically calculated based on the length of the the code_seq list
#' @param display_type a character string vector, the default is c("code", "output") for code and output to be displayed side-by-side, "output" will create spawned code chunks to only display output, "code" will create spawned code chunks only to show the partial code builds; "func" and "md" may also be displayed
#' @param lang a character string indicating what programming language will be used. "r" is default; "python" is experimental
#' @param func_seq a character string with function names; default is NULL and will reflect whatever function is highlighted from the code sequence
#' @param title a character string that may contain a title for the frames of the flipbook; this may included header info "## My Title" for example is a second level markdown title in Xaringan
#' @param md a character string vector that contains markdown; each element will be shown on a separate slide in the display panel "md" (see display_type)
#' @param md2 a character string vector that contains markdown; each element will be shown on a separate slide in the display panel "md" (see display_type)
#' @param replace a character string to be replaced in the input code sequentially with the replacement vector elements
#' @param replacements a character string vector to be replace the string indicated by the 'replace' parameter
#' @param replace2 a character string to be replaced in the input code sequentially with the replacement2 vector elements
#' @param replacements2 a character string vector to be replace the string indicated by the 'replace2' parameter
#' @param replace3 a character string to be replaced in the input code sequentially with the replacement3 vector elements
#' @param replacements3 a character string vector to be replace the string indicated by the 'replace3' parameter
#' @param widths a numeric vector containing relative widths for panels
#' @param font_size_code this ain't working yet!
#'
#' @return a string object is returned will only work in 'knitr' context
#' @export
#'
chunk_reveal <- function(chunk_name = NULL,
                   break_type = "auto",
                   left_assign = F,
                   left_assign_add = NULL,
                   lang = "r",
                   omit = "#OMIT",
                   code_seq = NULL,
                   code_seq_lag = NULL,
                   code_seq_lag2 = NULL,
                   code_seq_target = NULL,
                   code_seq_start = NULL,
                   func_seq = NULL,
                   num_breaks = NULL,
                   display_type = c("code", "output"),
                   title = "",
                   md = NULL,
                   md2 = NULL,
                   replacements = NULL,
                   replace = NULL,
                   replacements2 = replacements,
                   replace2 = replace,
                   replacements3 = replacements,
                   replace3 = replace,
                   widths = NULL,
                   float = "left",
                   chunk_options = "fig.width = 6",
                   color = c("black", "black", "black"),
                   font_size_code = "80%"
                   #,
                   # out.width = "70%",
                   # out.height = "70%"
                   ){

  correct_py(lang = lang)

  if (is.null(widths)){

    if (length(display_type) == 1) { widths <- c(1)}
    if (length(display_type) == 2) { widths <- c(39,60)}
    if (length(display_type) == 3) { widths <- c(29,39,30)}

  }

  if (!is.null(chunk_name) & is.null(code_seq)) {

    code_seq <- chunk_name_return_code_sequence(chunk_name = chunk_name,
                                                break_type = break_type,
                                                left_assign = left_assign,
                                                left_assign_add = left_assign_add,
                                                lang = lang,
                                                omit = omit,
                                                replace = replace, replacements = replacements,
                                                replace2 = replace2, replacements2 = replacements2,
                                                replace3 = replace3, replacements3 = replacements3)

  }

  if (is.null(func_seq) & !is.null(code_seq)){

    try(func_seq <- chunk_name_return_function_sequence(chunk_name, break_type, left_assign,
                                                        left_assign_add = left_assign_add,
                                                        lang = lang, omit = omit))
    #try because not worked out for python?
  }

  # for break_type equal 1, lag throws error so just try
  try(code_seq_lag <- code_seq_create_lag(code_seq = code_seq, lag = 1))
  try(code_seq_lag2 <- code_seq_create_lag(code_seq = code_seq, lag = 2))
  try(code_seq_target <- code_seq_create_target(code_seq = code_seq))
  try(code_seq_start <- code_seq_create_start(code_seq = code_seq))

  if (is.null(chunk_name)) {
   #randomly generated chunk_name if there is none
    chunk_name <- sample(1:100000, 1)
  }

  if (!is.null(code_seq)) {

    num_breaks <- length(code_seq)

  }

  if (is.null(num_breaks)){ # in case you have no code sequence

    num_breaks <- length(md)

    }

  if(is.null(chunk_name)){chunk_name <- sample(1000:9999, 1)}

  text <- chunk_expand(chunk_name = chunk_name,
                       break_type = break_type,
                       num_breaks = num_breaks,
                       display_type = display_type,
                       title = title,
                       lang = lang,
                       md = md,
                       md2 = md2,
                       func = func,
                       widths = widths,
                       float = float,
                       color = color,
                       font_size_code = font_size_code,
                       chunk_options = chunk_options
                       #,
                       #out.height = out.height,
                       #out.width = out.width
                       )

  paste(knitr::knit(text = text), collapse = "\n")

}


## returning code sequence as a vector
# create_injectable_code() %>%
#   code_parse() %>%
#   parsed_return_partial_code_sequence(break_type = "replacement",
#                                       replacements = 1:4,
#                                       replace = 10)


chunk_code_seq_as_vector <- function(chunk_name,
                               break_type = "auto",
                               left_assign = F,
                               left_assign_add = NULL,
                               lang = "r",
                               omit = "#OMIT",
                               replacements = NULL,
                               replace = NULL,
                               replacements2 = NULL,
                               replace2 = NULL,
                               replacements3 = NULL,
                               replace3 = NULL){

  chunk_name_return_code_sequence(chunk_name = chunk_name,
                                              break_type = break_type,
                                              left_assign = left_assign,
                                              left_assign_add = left_assign_add,
                                              lang = lang,
                                              omit = omit,
                                              replace = replace, replacements = replacements,
                                              replace2 = replace2, replacements2 = replacements2,
                                              replace3 = replace3, replacements3 = replacements3) %>%
    purrr::flatten_chr()

}







#' Function takes character string, splits it based on delimiter,
#' and returns each element of the resultant vector on its own slide
#'
#' @param text a character string to be split and delivered piece-wise to a slide
#' @param sep a character string to delimit the split of the input text
#' @param md_prefix a character string prefix to each markdown element, defaults to "#"
#' @param sep_replace a character string that will replace the delimiter, defaults to empty string ""
#' @param slide_break a character string containing slide break characters, defaults to "---" for xaringan slideshows
#' @param class a character string in which you can set the class, defaults to "class: inverse, middle, center"
#'
#' @return knit text to be interpreted as slides
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

  segments <- text_segment(text = text, sep = sep)

  text <- glue::glue(
    {slide_break},
    {class},
    "{md_prefix} {segments}{sep_replace}",
    "",
    .sep = "\n")

  paste(knitr::knit(text = text), collapse = "\n")

}




