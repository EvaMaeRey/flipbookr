local_code_non_sequential <- "cars %>%
mutate(speed_14_plus = speed >= 14) %>%
ggplot() +
aes(x = speed) +
aes(y = dist) +
# Describing what follows
geom_point(
size = 2,   #REVEAL2
alpha = .3, #REVEAL3
color = \"blue\", #REVEAL4
) +
aes(color = speed_14_plus)"


#' Supress some code and highlight some code
#'
#' @param code
#' @param which_supress an integer vector
#' @param which_highlight an integer vector
#'
#' @return
#' @export
#'
#' @examples
#' reveal_code_nonsequential(code = local_code_non_sequential)
reveal_code_nonsequential <- function(code,
                                        which_supress = 2:5,
                                        which_highlight = 6){

  parsed <- parse_code(code = code)

  parsed %>%
    dplyr::mutate(reveal = ifelse(dplyr::row_number() %in% which_supress, "", raw_code)) %>%
    dplyr::mutate(reveal = stringr::str_remove(reveal, "#REVEAL\\d+"))
    dplyr::mutate(highlight = ifelse(dplyr::row_number() %in% which_highlight, "#<<", "")) %>%
    dplyr::mutate(out = paste0(reveal, "  ", comment, highlight)) %>%
    dplyr::select(out) ->
    up_to_result
  up_to_result$out

}


#' Reveal chunk content non-sequentially
#'
#' @param chunk_name
#' @param which_supress an integer vector indicating rows of code to supress
#' @param which_highlight an integer vector indicating rows of code to highlight
#'
#' @return redacted code with highlighting indicators
#' @export
#'
#' @examples
reveal_chunk_nonsequential <- function(chunk_name,
                                       which_supress = 2:5,
                                       which_highlight = 6){

  content <- chunk_as_text(chunk_name)

  reveal_code_nonsequential(code = content,
                            which_supress = which_supress,
                            which_highlight = which_highlight)

}





#' Calculate a list of vectors for rows to supress at each step
#'
#' @param parsed parsed code including user_non_seq column
#'
#' @return a list of integer vectors
#' @export
#'
#' @examples
calc_supress <- function(parsed){

  parsed %>%
    dplyr::pull(user_non_seq) ->
    code_ordering

  code_ordering %>%
    unique() %>%
    sort() ->
    steps

  which_supress <- list()

  for (i in steps) {

    which_supress[[i]] <- which(code_ordering > i)

  }


  return(which_supress)

}



#' A list of vectors saying where to highlight at each step
#'
#' @param parsed parsed code including user_non_seq column
#'
#' @return a list of integer vectors
#' @export
#'
#' @examples
calc_highlight_non_sequential <- function(parsed) {

  parsed %>%
    pull(user_non_seq) ->
    code_ordering

  code_ordering %>%
    unique() %>%
    sort() ->
    steps

  highlighting <- list()

  for (i in steps)

    if (i == 1) {
      highlighting[[i]] <- NULL
    } else {
      highlighting[[i]] <- which(code_ordering == i)
    }

  return(highlighting)

}


#' Create non-sequentially revealed partial chunks
#'
#' @param chunk_name a character string which is a code chunk name
#' @param show_code a logical for whether to show the code
#' @param title a character string if a slide title is desired
#' @param reg_assignment a logical for if regular assignment is being used at the beginning of the slide - may not be working
#'
#' @return
#' @export
#'
#' @examples
non_sequential_partially_knit_chunks <- function(chunk_name,
                                                 show_code = T,
                                                 title = "",
                                                 reg_assignment = F) {

  # Create slide for lines 1:N for each line N in the given chunk

  parsed <- parse_chunk(chunk_name)

  parsed %>%
    pull(user_non_seq) %>%
    unique() %>%
    sort() ->
    steps

  steps_supress <- calc_supress(parsed)

  highlighting <- calc_highlight_non_sequential(parsed)

  if (show_code == T) {
    partial_knit_steps <- glue::glue(
      "class: split-40",
      "count: false",
      ".column[.content[",
      "```{r plot_{{chunk_name}}_{{steps}}, eval=FALSE, code=reveal_chunk_nonsequential('{{chunk_name}}', {{steps_supress}}, {{highlighting}})}",
      "```",
      "]]",
      ".column[.content[",
      "```{r output_{{chunk_name}}_{{steps}}, echo=FALSE, code=reveal_chunk_nonsequential('{{chunk_name}}', {{steps_supress}}, {{highlighting}})}",
      "```",
      "]]",
      .open = "{{", .close = "}}", .sep = "\n"
    )
  } else {

    partial_knit_steps <- glue::glue(title,"```{r output_{{chunk_name}}_{{steps}}, echo=FALSE, code=reveal_chunk_nonsequential('{{chunk_name}}', {{steps_supress}}, {{highlighting}})}",
                                     "```",
                                     .open = "{{", .close = "}}", .sep = "\n"
    )

  }

  glue::glue_collapse(x = partial_knit_steps, sep = "\n---\n")
}




#' Apply nonsequential reveal
#'
#' @param chunk_name a character string which is a code chunk name
#' @param show_code a logical for whether to show the code
#' @param title a character string if a slide title is desired
#' @param reg_assignment a logical for if regular assignment is being used at the beginning of the slide - may not be working
#'
#' @return character string to be interpreted as many code chunks by Rmarkdown in Xaringan
#' @export
#'
#' @examples
apply_reveal_nonsequential <- function(chunk_name,
                                       show_code = T,
                                       title = "",
                                       reg_assignment = F){

  paste(knitr::knit(
    text =
      non_sequential_partially_knit_chunks(
        chunk_name,
        show_code,
        title,
        reg_assignment)),
    collapse = "\n"
        )

}


