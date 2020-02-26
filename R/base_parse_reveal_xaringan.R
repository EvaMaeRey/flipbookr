# Emi Tanaka (@statsgen) and Garrick Aden-Buie (@grrrck) and Evangeline Reynolds (@EvaMaeRey)
# have contributed to this code
create_code <- function(){ # for testing w/o knitting

"cars %>%             # the data  #BREAK
  filter(speed > 4) %>%  # subset
  ggplot() +              # pipe to ggplot
  aes(x = speed) +
  aes(y = dist) +
  # Describing what follows
  geom_point(alpha = .3) + #BREAK
  geom_point(alpha = 1) + #BREAK2
  geom_jitter(alpha = .5) + #BREAK3
  aes(color =
  speed > 14
  ) %+%
  cars ->
  my_plot  #BREAK"

}


#' Title
#'
#' @return
#' @export
#'
#' @examples
create_short_code <- function(){ # for testing w/o knitting

  "cars %>%             # the data
  filter(speed > 4) %>%  # subset #BREAK
  ggplot() #BREAK"

}


create_ggplot_code <- function(){ # for testing w/o knitting

  "ggplot2::ggplot(cars) +  # initiate ggplot
  ggplot2::aes(x = speed) +
  ggplot2::aes(y = dist) +
  # Describing what follows
  ggplot2::geom_point(alpha = .3) "

}


create_left_assign_code <- function(){

# for testing w/o knitting
  "my_cars <- cars %>%             # the data  #BREAK
filter(speed > 4) %>%  # subset
ggplot() +              # pipe to ggplot
aes(x = speed) +
aes(y = dist) +
# Describing what follows
geom_point(alpha = .3)"

}


#' Code chunk as text
#'
#' @param chunk_name a character string which is a chunk name
#'
#' @return the code in the chunk as a string
chunk_code_get <- function(chunk_name){

  paste(knitr::knit_code$get(chunk_name), collapse = "\n")

}


code_as_table <- function(code){

  code %>%
    stringr::str_split(pattern = "\n") %>%
    .[[1]] %>%
    tibble::tibble(raw_code = .) %>%
    dplyr::mutate(line = 1:dplyr::n())

}


#' Parse code
#'
#' @param code code as a character string
#'
#' @return parsed code
#'
#' @examples
code_base_parse <- function(code) {

  sf <- srcfile(code)
  try(parse(text = code, srcfile = sf))
  utils::getParseData(sf)

}



# create_ggplot_code() %>%
# code_as_string()


base_parsed_count_parentheses <- function(base_parsed){

  base_parsed %>%
    dplyr::rename(line = line1) %>%
    dplyr::mutate(open_par = text == "(") %>%
    dplyr::mutate(closed_par = text == ")") %>%
    dplyr::mutate(open_curly = text == "{") %>%
    dplyr::mutate(closed_curly = text == "}") %>%
    dplyr::mutate(open_square = text == "[") %>%
    dplyr::mutate(open_square = ifelse(text == "[[", 2, open_square)) %>%
    dplyr::mutate(closed_square = text == "]") %>%
    dplyr::group_by(line) %>%
    dplyr::summarise(
      full_line = paste0(text, collapse = ""),
      comment = stringr::str_trim(paste0(ifelse(token == "COMMENT", text, ""),
                                         collapse = " ")),
      num_open_par = sum(open_par),
      num_closed_par = sum(closed_par),
      num_open_curly = sum(open_curly),
      num_closed_curly = sum(closed_curly),
      num_open_square = sum(open_square),
      num_closed_square = sum(closed_square)
              ) %>%
    dplyr::mutate(balanced_paren = (cumsum(num_open_par) - cumsum(num_closed_par)) == 0) %>%
    dplyr::mutate(balanced_curly = (cumsum(num_open_curly) - cumsum(num_closed_curly)) == 0) %>%
    dplyr::mutate(balanced_square = (cumsum(num_open_square) - cumsum(num_closed_square)) == 0)
}


code_parse <- function(code){

  raw_code_table <-
    code %>%
    code_as_table()

  code %>%
  code_base_parse() %>%
    base_parsed_count_parentheses() %>%
    dplyr::left_join(raw_code_table) %>%
    dplyr::mutate(code = ifelse(comment != "", stringr::str_remove(raw_code, comment), raw_code)) %>%
    dplyr::mutate(connector = stringr::str_extract(stringr::str_trim(code), "%>%$|\\+$|->$|%\\+%|%\\$%")) %>%
    dplyr::mutate(connector = tidyr::replace_na(connector, "")) %>%
    dplyr::mutate(non_seq = stringr::str_extract(comment, "#BREAK-?\\d+")) %>%
    dplyr::mutate(non_seq = stringr::str_extract(non_seq, "-?\\d+")) %>%
    dplyr::mutate(non_seq = as.numeric(non_seq)) %>%
    dplyr::mutate(non_seq = tidyr::replace_na(non_seq, 1)) %>%
    dplyr::mutate(comment = stringr::str_remove(comment, "#BREAK\\d+")) %>%
    dplyr::mutate(user = stringr::str_detect(comment, "#BREAK")) %>%
    dplyr::mutate(comment = stringr::str_remove(comment, "#BREAK")) %>%
    dplyr::mutate(code = stringr::str_remove(stringi::stri_trim_right(code), "%>%$|\\+$|->$|%\\+%|%\\$%")) %>%
    dplyr::mutate(auto = balanced_paren & balanced_curly & balanced_square &
                    code != "") %>%
    dplyr::select(line, raw_code, code, connector, comment, auto, user, non_seq)

}



parsed_calc_show <- function(parsed, break_type = "user"){

  if (break_type == "auto") {

    code_order <- cumsum(parsed$auto) + 1 - parsed$auto
    num_panes <- max(code_order)

  } else if (break_type == "user") {

    code_order <- cumsum(parsed$user) + 1 - parsed$user
    num_panes <- max(code_order)

  } else if (break_type == "non_seq") {

    # make flexible by allowing non integers here.
    code_order <- parsed$non_seq
    num_panes <- max(abs(code_order)) # Matt Gambino change to account for negative as max value

  } else if (is.numeric(break_type)) {  # multiverse case

    code_order <- rep(1, nrow(parsed))
    num_panes <- break_type

  }

  which_show <- list()

  for (i in 1:num_panes) {

    # fix this for non_sequential to allow removal
    which_show[[i]] <- which(code_order <= i)
    # Matt Gambino: change pipes to second statement to drop negative values
    # which_show[[i]] <-
    #   which( code_order <= i ) %>%
    #   .[!. %in% which( code_order >= -i & code_order < 0 )]

  }

  which_show

}


# shown_lines_calc_highlight()
shown_lines_calc_highlight <- function(which_show = list(c(1, 2), c(1, 2, 3, 4)),
                                    break_type = "auto"){

  which_highlight <- list()

  if (break_type == "user" | break_type == "auto") {

  which_highlight[[1]] <- which_show[[1]]

    for (i in 2:length(which_show)) {

      which_highlight[[i]] <- which_show[[i]][!(which_show[[i]] %in% which_show[[i - 1]])]

    }

  }  else if (is.numeric(break_type)) {

      for (i in 1:length(which_show)) {

        which_highlight[[i]] <- as.integer(c())

      }

  } else if (break_type == "non_seq") {


    which_highlight[[1]] <- as.integer(c())

    for (i in 2:length(which_show)) {

      which_highlight[[i]] <- which_show[[i]][!(which_show[[i]] %in% which_show[[i - 1]])]

    }


    }

  which_highlight

  }


parsed_return_partial_code <- function(parsed,
                                            which_show_frame = 1:3,
                                            which_highlight_frame = 3){

  parsed %>%
    dplyr::filter(1:dplyr::n() %in% which_show_frame) %>%
    dplyr::mutate(connector = dplyr::case_when(1:dplyr::n() == dplyr::n() ~ "",
                                               1:dplyr::n() != dplyr::n() ~ connector)) %>%
    dplyr::mutate(highlight = ifelse(1:dplyr::n() %in% which_highlight_frame, "#<<", "" )) %>%
    dplyr::mutate(out = paste0(code, "", connector, "  ", comment, highlight)) %>%
    dplyr::pull()

}


parsed_left_assign_return_partial_code <- function(parsed,
                                               which_show_frame = 1:3,
                                               which_highlight_frame = 3){

  the_reveal <- parsed_return_partial_code(parsed,
                                                which_show_frame,
                                                which_highlight_frame)

  the_reveal[1] %>%
      stringr::str_extract(".+\\<-") %>%
      stringr::str_remove("<-") %>%
      stringr::str_trim() ->
    object_to_track  # this is the object created at the beginning of the code chunk

    c(the_reveal, " ", paste(object_to_track, "# tracked object"))

}


#' Title
#'
#' @param parsed
#' @param break_type
#' @param which_show
#' @param which_highlight
#' @param left_assign
#'
#' @return
#' @export
#'
#' @examples
parsed_return_partial_code_sequence <- function(parsed,
                         break_type = "auto",
                         which_show = parsed_calc_show(parsed = parsed,
                                                       break_type = break_type),
                         which_highlight =
                           shown_lines_calc_highlight(which_show = which_show,
                                                      break_type = break_type),
                         left_assign = F){

  partial_code_frames <- list()

  for (i in 1:length(which_show)) {


  if (left_assign == F) {
    partial_code_frames[[i]] <-
      parsed_return_partial_code(parsed,
                                 which_show_frame = which_show[[i]],
                                 which_highlight_frame = which_highlight[[i]])
  }else{
    partial_code_frames[[i]] <-
      parsed_left_assign_return_partial_code(parsed,
                                            which_show_frame = which_show[[i]],
                                            which_highlight_frame = which_highlight[[i]])
  }

  }

  partial_code_frames

}


#' Title
#' @return
#' @export
chunk_name_return_code_sequence <- function(chunk_name,
                                            break_type = "auto",
                                            left_assign = F){

chunk_name %>%
  chunk_code_get() %>%
  code_parse() %>%
  parsed_return_partial_code_sequence(break_type = break_type,
                                 left_assign = left_assign)

}





#' Title
#'
#' @param display_type
#' @param echo
#' @param eval
#'
#' @return
#' @export
#'
#' @examples
#' return_partial_chunks_template_code()
return_partial_chunks_template_code <- function(){

  "```{r {{{chunk_name}}}_{{{break_type}}}_{{{breaks}}}_code, eval = FALSE, echo = TRUE, code = code_seq[[{{{breaks}}}]]}
  ```"

}



# Thinking about delivery to sweave/beamer
# return_partial_chunks_template_code <-
#   function(type = "code", delivery = "rmd") {
#
#     chunk_name_and_options <-
#       glue::glue(
#         "{{{chunk_name}}}_{{{break_type}}}_{{{breaks}}}_{{{{type}}}},",
#         "eval = FALSE, echo = TRUE, code = code_seq[[{{{breaks}}}]]",
#         .open = "{{{{",
#         .close = "}}}}"
#       )
#
#     if (delivery == "rmd" | delivery == "Rmd") {
#       paste0("```{r}", chunk_name_and_options, "\n```")
#
#     } else if (delivery == "sweave") {
#       paste0("<<", chunk_name_and_options, ">>=\n@")
#
#
#     }
#
#   }



#' Title
#'
#' @return
#' @export
#'
#' @examples
#' return_partial_chunks_template_output()
return_partial_chunks_template_output <- function(){

"```{r {{{chunk_name}}}_{{{break_type}}}_{{{breaks}}}_output, eval = TRUE, echo = FALSE, code = code_seq[[{{{breaks}}}]]}
```"

}



#' Title
#'
#' @param chunk_name
#' @param break_type
#' @param left_assign
#' @param display_type
#' @param split
#'
#' @return
#' @export
#'
#' @examples
#' chunk_expand()
#' chunk_expand(break_type = "user", display_type = "code")
#' chunk_expand(break_type = "non_seq", display_type = "output")
chunk_expand <- function(chunk_name = "example",
                                  break_type = "auto",
                                  display_type = "both",
                                  num_breaks = 2,
                                  split = 40){

breaks <- 1:num_breaks

if (display_type == "both") {

  partial_knit_steps <- glue::glue(
    "class: split-{{{split}}}",
    "count: false",
    ".column[.content[",
    return_partial_chunks_template_code(),
    "]]",
    ".column[.content[",
    return_partial_chunks_template_output(),
    "]]",
    " ",
    .open = "{{{", .close = "}}}", .sep = "\n"
  )

} else if (display_type == "code" | display_type == "output") {

  if (display_type == "output") {

    chunk <- return_partial_chunks_template_output()

  } else {

    chunk <- return_partial_chunks_template_code()

  }

  partial_knit_steps <- glue::glue(
    "count: false",
    chunk,
    .open = "{{{", .close = "}}}", .sep = "\n"
  )

}

glue::glue_collapse(x = partial_knit_steps, sep = "\n---\n")

}







#' Title
#'
#' @param chunk_name
#' @param break_type
#' @param left_assign
#' @param code_seq
#' @param num_breaks
#' @param display_type
#' @param split
#'
#' @return
#' @export
#'
#' @examples
chunk_reveal <- function(chunk_name = "example_name",
                   break_type = "auto",
                   left_assign = F,
                   code_seq = chunk_name_return_code_sequence(chunk_name, break_type, left_assign),
                   num_breaks = length(code_seq),
                   display_type = "both",
                   split = 40){

  text <- chunk_expand(chunk_name = chunk_name,
                       break_type = break_type,
                       num_breaks = num_breaks,
                       display_type = display_type,
                       split = split)

  paste(knitr::knit(text = text), collapse = "\n")


}



