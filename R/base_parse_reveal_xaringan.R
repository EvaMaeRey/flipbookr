# Emi Tanaka (@statsgen) and Garrick Aden-Buie (@grrrck) and Evangeline Reynolds (@EvaMaeRey)
# have contributed to this code
create_local_code <- function(){ # for testing w/o knitting

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

}


#' Title
#'
#' @return
#' @export
#'
#' @examples
create_local_code_short <- function(){ # for testing w/o knitting

  "cars %>%             # the data
  filter(speed > 4) %>%  # subset #REVEAL
  ggplot() #REVEAL"

}



create_local_code_regular_assignment <- function(){

# for testing w/o knitting
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

}


#' Code chunk as text
#'
#' @param chunk_name a character string which is a chunk name
#'
#' @return the code in the chunk as a string
chunk_as_text <- function(chunk_name){

  paste(knitr::knit_code$get(chunk_name), collapse = "\n")

}


#' Parse code
#'
#' @param code code as a character string
#'
#' @return parsed code
#'
#' @examples
#' local_code <- create_local_code()
#' parse_code(code = local_code)
parse_code <- function(code) {

  raw_code_table <- tibble::tibble(raw_code =
                                     stringr::str_split(code, "\n")[[1]]) %>%
    dplyr::mutate(line = 1:dplyr::n())

  sf <- srcfile(code)
  try(parse(text = code, srcfile = sf))
  utils::getParseData(sf) %>%
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
    dplyr::left_join(raw_code_table) %>%
    dplyr::mutate(code = ifelse(comment != "", stringr::str_remove(raw_code, comment), raw_code)) %>%
    dplyr::mutate(connector = stringr::str_extract(stringr::str_trim(code), "%>%$|\\+$|->$|%\\+%")) %>%
    dplyr::mutate(connector = tidyr::replace_na(connector, "")) %>%
    dplyr::mutate(non_seq = stringr::str_extract(comment, "#REVEAL-?\\d+")) %>%
    dplyr::mutate(non_seq = stringr::str_extract(non_seq, "-?\\d+")) %>%
    dplyr::mutate(non_seq = as.numeric(non_seq)) %>%
    dplyr::mutate(non_seq = tidyr::replace_na(non_seq, 1)) %>%
    dplyr::mutate(comment = stringr::str_remove(comment, "#REVEAL\\d+")) %>%
    dplyr::mutate(user = stringr::str_detect(comment, "#REVEAL")) %>%
    dplyr::mutate(comment = stringr::str_remove(comment, "#REVEAL")) %>%
    dplyr::mutate(code = stringr::str_remove(stringi::stri_trim_right(code), "%>%$|\\+$|->$|%\\+%")) %>%
    dplyr::mutate(balanced_paren = (cumsum(num_open_par) - cumsum(num_closed_par)) == 0) %>%
    dplyr::mutate(balanced_curly = (cumsum(num_open_curly) - cumsum(num_closed_curly)) == 0) %>%
    dplyr::mutate(balanced_square = (cumsum(num_open_square) - cumsum(num_closed_square)) == 0) %>%
    dplyr::mutate(auto = balanced_paren & balanced_curly & balanced_square &
             code != "") %>%
    dplyr::select(line, raw_code, code, connector, comment, auto, user, non_seq)

}




calc_lines_to_show <- function(parsed, break_type = "user"){

  if (break_type == "auto") {

    code_order <- cumsum(parsed$auto) + 1 - parsed$auto
    num_panes <- max(code_order)

  } else if (break_type == "user") {

    code_order <- cumsum(parsed$user) + 1 - parsed$user
    num_panes <- max(code_order)

  } else if (break_type == "non_seq") {

    # make flexible by allowing non integers here.
    code_order <- parsed$non_seq
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


# calc_lines_to_highlight()
calc_lines_to_highlight <- function(which_show = list(c(1, 2), c(1, 2, 3, 4)),
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


show_and_highlight_frame_classic <- function(parsed,
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


show_and_highlight_frame_reg_assign <- function(parsed,
                                               which_show_frame = 1:3,
                                               which_highlight_frame = 3){

  the_reveal <- show_and_highlight_frame_classic(parsed,
                                                which_show_frame,
                                                which_highlight_frame)

  the_reveal[1] %>%
      stringr::str_extract(".+\\<-") %>%
      stringr::str_remove("<-") %>%
      stringr::str_trim() ->
    object_to_track  # this is the object created at the beginning of the code chunk

    c(the_reveal, " ", paste(object_to_track, "# print object"))

}


#' Title
#'
#' @param parsed
#' @param break_type
#' @param which_show
#' @param which_highlight
#' @param reg_assign
#'
#' @return
#' @export
#'
#' @examples
show_and_highlight_code_frames <- function(parsed,
                         break_type = "auto",
                         which_show = calc_lines_to_show(parsed = parsed, break_type = break_type),
                         which_highlight = calc_lines_to_highlight(which_show = which_show, break_type = break_type),
                         reg_assign = F){

  partial_code_frames <- list()

  for (i in 1:length(which_show)) {


  if (reg_assign == F) {
    partial_code_frames[[i]] <- show_and_highlight_frame_classic(parsed, which_show_frame = which_show[[i]], which_highlight_frame = which_highlight[[i]])
  }else{
    partial_code_frames[[i]] <- show_and_highlight_frame_reg_assign(parsed, which_show_frame = which_show[[i]], which_highlight_frame = which_highlight[[i]])
  }

  }

  partial_code_frames

}



get_partial_codes_from_chunk <- function(chunk_name,
                                         break_type = "auto",
                                         reg_assign = F){

chunk_name %>%
  chunk_as_text() %>%
  parse_code() %>%
  show_and_highlight_code_frames(break_type = break_type,
                                 reg_assign = reg_assign)

}



return_partial_chunks_template <- function(display_type = "output",
                                           break_type = "auto",
                                  eval = display_type == "output",
                                  echo = display_type == "code") {

  glue::glue("```{r {{chunk_name}}_{{{break_type}}}_{{breaks}}_{{{display_type}}}, eval={{{eval}}}, echo = {{{echo}}}, code = the_code[[{{breaks}}]]}",
             "```",
             .open = "{{{", .close = "}}}", .sep = "\n")
}




return_partial_code_or_output_chunks <- function(chunk_name = "a_chunk_name",
                                                 break_type = "auto",
                                                 reg_assign = F,
                                                 the_code = get_partial_codes_from_chunk(chunk_name = chunk_name,
                                                                              break_type = break_type,
                                                                              reg_assign = reg_assign),
                                                 display_type = "output") {



  breaks <- 1:length(the_code)  # number of temporal breakpoints


  partial_knit_steps <- glue::glue(
    "count: false",
    return_partial_chunks_template(eval = display_type == "output",
                                   echo = display_type == "code",
                                   display_type = display_type,
                                   break_type = break_type),
    .open = "{{", .close = "}}", .sep = "\n"
  )

  glue::glue_collapse(x = partial_knit_steps, sep = "\n---\n")

}



return_partial_side_by_side_code_output_chunks <- function(chunk_name = "a_chunk_name",
                                                           break_type = "auto",
                                                           reg_assign = F,
                                                           the_code = get_partial_codes_from_chunk(chunk_name = chunk_name,
                                                                                       break_type = break_type,
                                                                                       reg_assign = reg_assign),
                                                           split = 40) {

  breaks <- 1:length(the_code)  # number of temporal breakpoints

  partial_knit_steps <- glue::glue(
    "class: split-{{split}}",
    "count: false",
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

#' Title
#'
#' @param the_code
#' @param chunk_name
#' @param break_type
#' @param reg_assign
#' @param display_type
#' @param split
#'
#' @return
#' @export
#'
#' @examples
partially_knit_chunks <- function(the_code = get_partial_codes_from_chunk(chunk_name = chunk_name,
                                                                          break_type = break_type,
                                                                          reg_assign = reg_assign),
                                  chunk_name = "example_chunk_name",
                                  break_type = "auto",
                                  reg_assign = F,

                                  display_type = "both",
                                  split = 40){


if (display_type == "both") {

  return_partial_side_by_side_code_output_chunks(chunk_name = chunk_name,
                                                 break_type = break_type,
                                                 the_code = the_code,
                                                 split = split)

} else {

  return_partial_code_or_output_chunks(chunk_name = chunk_name,
                                       break_type = break_type,
                                       the_code = the_code,
                                       display_type = display_type)

}

}


#' reveal in Rmarkdown file, to be used in-line
#'
#' @param chunk_name a character string which is a chunk name
#' @param break_type "auto", "user", "non_seq" or numeric
#' @param display_type string "both", "code", "output", default is "both"
#' @param the_code list of string vectors containing the partial code, computes automatically
#' @param reg_assign logical set to T if output of some object created at beginning of code chunk should be displayed
#' @param split percent split between code and output if both
#'
#' @return a character string to be interpreted as .Rmd content
#' @export
#'
#' @examples
reveal <- function(chunk_name,
                   display_type = "both",
                   break_type = "auto",
                   the_code = get_partial_codes_from_chunk(chunk_name, break_type, reg_assign),
                   reg_assign = F,
                   split = 40){

  paste(knitr::knit(text =
                      partially_knit_chunks(chunk_name = chunk_name,
                                            the_code = the_code,
                                            reg_assign = reg_assign,
                                            display_type = display_type,
                                            break_type = break_type,
                                            split = split)),
        collapse = "\n")
}

