# Emi Tanaka (@statsgen) and Garrick Aden-Buie (@grrrck) and Evangeline Reynolds (@EvaMaeRey)
# have contributed to this code

local_code <- # for testing w/o knitting
  "cars %>%             # the data  #REVEAL
filter(speed > 4) %>%  # subset
ggplot() +              # pipe to ggplot
aes(x = speed) +
aes(y = dist) +
# Describing what follows
geom_point(alpha = .3) + #REVEAL
geom_point(alpha = 1) + #REVEAL2
geom_jitter(alpha = .5) + #REVEAL3
aes(color =
speed > 14
) %+%
cars ->
my_plot  #REVEAL"


local_code_regular_assignment <- # for testing w/o knitting
  "my_cars <- cars %>%             # the data  #REVEAL
filter(speed > 4) %>%  # subset
ggplot() +              # pipe to ggplot
aes(x = speed) +
aes(y = dist) +
# Describing what follows
geom_point(alpha = .3) + #REVEAL
aes(color =
paste(\"speed\",
speed > 14)
) %+%
cars"



#' Code chunk as text
#'
#' @param chunk_name a character string which is a chunk name
#'
#' @return the code in the chunk as a string
#' @export
#'
#' @examples
chunk_as_text <- function(chunk_name){

  paste(knitr::knit_code$get(chunk_name), collapse = "\n")

}


# New - using parser
# We want to take just text (scalar), parse it and return a useful dataframe

#' Parse code
#'
#' @param code code as a character string
#'
#' @return parsed code
#' @export
#'
#' @examples
#' parse_code(code = local_code)
parse_code <- function(code) {

  # code <- paste(knitr::knit_code$get("the_code"), collapse = "\n")
  # code <- local_code

  raw_code_table <- tibble::tibble(raw_code =
                                     stringr::str_split(code, "\n")[[1]]) %>%
    dplyr::mutate(line = 1:dplyr::n())

  sf <- srcfile(code)
  try(parse(text = code, srcfile = sf))
  getParseData(sf) %>%
    dplyr::rename(line = line1) %>%
    dplyr::mutate(open_par = text == "(") %>%
    dplyr::mutate(closed_par = text == ")") %>%
    dplyr::mutate(open_curly = text == "{") %>%
    dplyr::mutate(closed_curly = text == "}") %>%
    dplyr::mutate(open_square = text == "[") %>%
    dplyr::mutate(open_square = ifelse(text == "[[", 2, open_square)) %>%
    dplyr::mutate(closed_square = text == "]") %>%
    # dplyr::mutate(num_open_par = stringr::str_count(token, "\\(|\\{|\\[")) %>% # Counting open parentheses
    # dplyr::mutate(num_closed_par = stringr::str_count(token, "\\)|\\}|\\]"))  %>% # Counting closed parentheses
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
    # dplyr::summarise(num_open_par = sum(num_open_par),
    #           num_closed_par = sum(num_closed_par),
    #           full_line = paste0(text, collapse = ""),
    #           comment = stringr::str_trim(paste0(ifelse(token == "COMMENT", text, ""),
    #                                     collapse = " "))) %>%
    dplyr::left_join(raw_code_table) %>%
    dplyr::mutate(code = ifelse(comment != "", stringr::str_remove(raw_code, comment), raw_code)) %>%
    dplyr::mutate(connector = stringr::str_extract(stringr::str_trim(code), "%>%$|\\+$|->$|%\\+%")) %>%
    dplyr::mutate(connector = tidyr::replace_na(connector, "")) %>%
    dplyr::mutate(user_non_seq = stringr::str_extract(comment, "#REVEAL-?\\d+")) %>%
    dplyr::mutate(user_non_seq = stringr::str_extract(user_non_seq, "-?\\d+")) %>%
    dplyr::mutate(user_non_seq = as.numeric(user_non_seq)) %>%
    dplyr::mutate(user_non_seq = tidyr::replace_na(user_non_seq, 1)) %>%
    dplyr::mutate(comment = stringr::str_remove(comment, "#REVEAL\\d+")) %>%
    dplyr::mutate(user_reveal = stringr::str_detect(comment, "#REVEAL")) %>%
    dplyr::mutate(comment = stringr::str_remove(comment, "#REVEAL")) %>%
    dplyr::mutate(code = stringr::str_remove(stringi::stri_trim_right(code), "%>%$|\\+$|->$|%\\+%")) %>%
    dplyr::mutate(balanced_paren = (cumsum(num_open_par) - cumsum(num_closed_par)) == 0) %>%
    dplyr::mutate(balanced_curly = (cumsum(num_open_curly) - cumsum(num_closed_curly)) == 0) %>%
    dplyr::mutate(balanced_square = (cumsum(num_open_square) - cumsum(num_closed_square)) == 0) %>%
    dplyr::mutate(balanced_par = balanced_paren & balanced_curly & balanced_square &
             code != "") %>%
    dplyr::select(line, raw_code, code, connector, comment, user_non_seq, user_reveal, balanced_par)
    # dplyr::mutate(balanced_par = (cumsum(num_open_par) - cumsum(num_closed_par)) == 0 &
    #          code != "")

}
# parse_code(code = local_code)




calc_which_lines_to_show <- function(parsed, break_type = "user"){

  if (break_type == "auto") {

    code_order <- cumsum(parsed$balanced_par) + 1 - parsed$balanced_par
    num_panes <- max(code_order)

  } else if (break_type == "user") {

    code_order <- cumsum(parsed$user_reveal) + 1 - parsed$user_reveal
    num_panes <- max(code_order)

  } else if (break_type == "non_seq") {

    # make flexible by allowing non integers here.
    code_order <- parsed$user_non_seq
    num_panes <- max(code_order)

  } else if (is.numeric(break_type)) {  # multiverse case


    code_order <- rep(1, nrow(parsed))
    num_panes <- break_type

  }

  which_show <- list()

  for (i in 1:num_panes) {

    # fix this for non_sequential to allow removal
    which_show[[i]] <- which(code_order <= i)

  }

  which_show

}
# calc_which_lines_to_show(parsed = parse_code(local_code), break_type = "user")
# calc_which_lines_to_show(parsed = parse_code(local_code), break_type = "auto")
# calc_which_lines_to_show(parsed = parse_code(local_code), break_type = "non_seq")
# calc_which_lines_to_show(parsed = parse_code(local_code), break_type = 6)

# calc_lines_to_highlight()
calc_lines_to_highlight <- function(which_show = list(c(1,2), c(1,2,3,4)), break_type = "auto"){


  which_highlight <- list()

  if (break_type == "user" | break_type == "auto" | break_type == "non_seq"){

  which_highlight[[1]] <- which_show[[1]]

    for (i in 2:length(which_show)) {

      which_highlight[[i]] <- which_show[[i]][!(which_show[[i]] %in% which_show[[i - 1]])]

    }

  }  else if (is.numeric(break_type)) {

      for (i in 1:length(which_show)) {

        which_highlight[[i]] <- as.integer(c())

      }

    }

  which_highlight

  }

# local_code %>%
#   parse_code() %>%
#   calc_which_lines_to_show(break_type = "non_seq") %>%
#   calc_lines_to_highlight(break_type = "non_seq")
#





# calc_lines_to_highlight()


#' Partially reveal parsed code
#'
#' @param parsed the output resulting from parsing code
#' @param break_point a integer indicating the line of code
#' @param highlight integers indicating which lines of code to highlight
#'
#' @return Partial code with indicators for highlight
#' @export
#'
#' @examples
show_and_highlight_pane_classic <- function(parsed, which_show = 1:3, which_highlight = 3){

  parsed %>%
    dplyr::filter(1:n() %in% which_show) %>%
    dplyr::mutate(connector = dplyr::case_when(1:dplyr::n() == dplyr::n() ~ "",
                                               1:dplyr::n() != dplyr::n() ~ connector)) %>%
    dplyr::mutate(highlight = ifelse(1:dplyr::n() %in% which_highlight, "#<<", "" )) %>%
    dplyr::mutate(out = paste0(code, "", connector, "  ", comment, highlight)) %>%
    dplyr::pull()


}

#' #' reveal_code_nonsequential(code = local_code_non_sequential)	#' #' reveal_code_nonsequential(code = local_code_non_sequential)
#' reveal_code_nonsequential <- function(code = local_code_non_sequential,	#' reveal_code_nonsequential <- function(code = local_code_non_sequential,
#'                                       which_supress = 2:5,	#'                                         which_supress = 2:5,
#'                                       which_highlight = 6){	#'                                         which_highlight = 6){
#'
#'   #'
#'   parsed <- parse_code(code = code)	#'   parsed <- parse_code(code = code)
#'
#'   #'
#'   parsed %>%	#'   parsed %>%
#'     dplyr::mutate(reveal = ifelse(dplyr::row_number() %in% which_supress, "", raw_code)) %>%	#'     dplyr::mutate(reveal = ifelse(dplyr::row_number() %in% which_supress, "", raw_code)) %>%
#'     dplyr::mutate(reveal = stringr::str_remove(reveal, "#REVEAL\\d+")) %>%	#'     dplyr::mutate(reveal = stringr::str_remove(reveal, "#REVEAL\\d+")) %>%
#'     dplyr::mutate(highlight = ifelse(dplyr::row_number() %in% which_highlight, "#<<", "")) %>%	#'     dplyr::mutate(highlight = ifelse(dplyr::row_number() %in% which_highlight, "#<<", "")) %>%
#'     dplyr::mutate(out = paste0(reveal, "  ", comment, highlight)) %>%	#'     dplyr::mutate(out = paste0(reveal, "  ", comment, highlight)) %>%
#'     dplyr::select(out) %>%	#'     dplyr::select(out) %>%
#'     dplyr::filter(!str_detect(out, "^\\s+$")) ->	#'     dplyr::filter(!str_detect(out, "^\\s+$")) ->
#'     up_to_result	#'     up_to_result
#'   up_to_result$out	#'   up_to_result$out
#'
#'   #'
#'                                       }


# example
show_and_highlight_pane_reg_assign <- function(parsed, which_show = 1:3, which_highlight = 3){

  the_reveal <- show_and_highlight_pane_classic(parsed, which_show, which_highlight)

  the_reveal[1] %>%
      stringr::str_extract(".+\\<-") %>%
      stringr::str_remove("<-") %>%
      stringr::str_trim() ->
    object_to_track  # this is the object created at the beginning of the code chunk

    c(the_reveal, " ", paste(object_to_track, "# print object"))

}

# example reveal_pane(parsed = parse_code(local_code))
partial_code <- function(parsed, which_show = 1:3, which_highlight = 3, reg_assign = F){

  if (reg_assign == F) {
    show_and_highlight_pane_classic(parsed, which_show, which_highlight)
  }else{
    show_and_highlight_pane_reg_assign(parsed, which_show, which_highlight)
  }

}


partial_chunk <- function(chunk_name, which_show = 1:3, which_highlight = 3, reg_assign = F){

  chunk_name %>%
    chunk_as_text() %>%
    parse_code() %>%
    partial_code(which_show, which_highlight, reg_assign)

}

return_partial_chunks_template <- function(display_type = "output",
                                           break_type = "auto",
                                  eval = display_type == "output",
                                  echo = display_type == "code") {

  glue::glue("```{r {{{display_type}}}_{{chunk_name}}_{{breaks}}_{{{break_type}}}, eval={{{eval}}}, echo = {{{echo}}}, code=partial_chunk('{{chunk_name}}', which_show = {{which_show}}, which_highlight = {{which_highlight}}, reg_assign = {{reg_assign}})}",
             "```",
             .open = "{{{", .close = "}}}", .sep = "\n")
}
# return_partial_chunks_template()



return_partial_side_by_side_code_output_chunks <- function(chunk_name = "a_chunk_name",
                                                           break_type = "auto",
                                           which_show = list(1, 1:2, 1:3),
                                           which_highlight = list(1, 2, 3),
                                           title = "My Title",
                                           reg_assign = F,
                                           split = 40) {

  breaks <- 1:length(which_show)  # number of temporal breakpoints

  partial_knit_steps <- glue::glue(
    "class: split-{{split}}",
    "count: false",
    "{{title}}",
    ".column[.content[",
    return_partial_chunks_template(display_type = "code",
                                   break_type = break_type),
    "]]",
    ".column[.content[",
    return_partial_chunks_template(display_type = "output",
                                   break_type = break_type),
    "]]",
    " ",
    .open = "{{", .close = "}}", .sep = "\n"
    )

  glue::glue_collapse(x = partial_knit_steps, sep = "\n---\n")

}
# return_partial_side_by_side_code_output_chunks()



return_partial_code_or_output_chunks <- function(chunk_name = "a_chunk_name",
                                                 break_type = "auto",
                                                 which_show = list(1, 1:2, 1:3),
                                                 which_highlight = list(1, 2, 3),
                                               title = "My Title",
                                               reg_assign = F,
                                               display_type = "output") {

  breaks <- 1:length(which_show)  # number of temporal breakpoints


  partial_knit_steps <- glue::glue(
    "count: false",
    "{{title}}",
    return_partial_chunks_template(eval = display_type == "output",
                                   echo = display_type == "code",
                                   display_type = display_type,
                                   break_type = break_type),
    .open = "{{", .close = "}}", .sep = "\n"
  )

  glue::glue_collapse(x = partial_knit_steps, sep = "\n---\n")

}
# return_partial_code_or_output_chunks()



# uses above code, but calculates breaks and highlight
partially_knit_chunks <- function(chunk_name = "example_chunk_name",
                                  title = "My Title",
                                  reg_assign = F,
                                  display_type = "both",
                                  break_type = "auto",
                                  split = 40){


  chunk_name %>%
    chunk_as_text() %>%
    parse_code() ->
  parsed

which_show <- calc_which_lines_to_show(parsed = parsed, break_type)

which_highlight <- calc_lines_to_highlight(which_show = which_show,
                                           break_type = break_type)

if (display_type == "both") {

  return_partial_side_by_side_code_output_chunks(chunk_name,
                                                 title,
                                                 break_type = break_type,
                                                 which_show = which_show,
                                                 which_highlight = which_highlight,
                                                 reg_assign = reg_assign,
                                                 split = 40)

} else {

  return_partial_code_or_output_chunks(chunk_name,
                                       title,
                                       display_type,
                                       break_type = break_type,
                                       which_show = which_show,
                                       which_highlight = which_highlight,
                                       reg_assign = reg_assign)

}

}
# partially_knit_chunks()


#' Apply reveal in Rmarkdown file, to be used in line
#'
#' @param chunk_name a character string which is a chunk name
#' @param user_reveal a logical for if breaks should be automatically determined or have been defined manually with "#REVEAL" message
#' @param show_code a logical for if the code should be displayed or not, default is TRUE
#' @param title a character string for a title for all the slides to display code-output evolution, default is an empty string
#' @param reg_assign logical set to T if output of some object created at beginning of code chunk should be displayed
#'
#' @return a character string to be interpreted as .Rmd content
#' @export
#'
#' @examples
apply_reveal <- function(chunk_name, display_type = "both", break_type = "auto", title = "", reg_assign = F, split = 40){

  paste(knitr::knit(text =
                      partially_knit_chunks(chunk_name,
                                            title,
                                            reg_assign = reg_assign,
                                            display_type,
                                            break_type = break_type,
                                            split)),
        collapse = "\n")
}

