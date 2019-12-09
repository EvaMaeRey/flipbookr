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
aes(color =
speed > 14
) %+%
cars ->
my_plot "

local_code_logical_indexing <-
"list(thing_1 = \"a\",
     thing_2 = matrix(data = 1:5, nrow = 2)) ->
my_named_list
list(\"a\",
1:5,
my_named_list) %>%
.[[ 3 ] ] %>%
.$thing_2 %>%
.[3]"


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

local_code_non_sequential <- "cars %>%
mutate(speed_14_plus = speed >= 14) %>%
ggplot() +
aes(x = speed) +
aes(y = dist) +
# Describing what follows
geom_point(
size = 2,   #REVEAL2
alpha = .3, #REVEAL3
color = \"blue\", #REVEAL-3
) +
aes(color = speed_14_plus)"


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
#' parse_code(code = local_code_non_sequential)
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
    dplyr::mutate(user_non_seq = stringr::str_extract(comment, "#REVEAL-?\\d+")) %>%
    dplyr::mutate(user_non_seq = stringr::str_extract(user_non_seq, "-?\\d+")) %>%
    dplyr::mutate(user_non_seq = as.numeric(user_non_seq)) %>%
    dplyr::mutate(user_non_seq = tidyr::replace_na(user_non_seq, 1)) %>%
    dplyr::mutate(user_reveal = stringr::str_detect(comment, "#REVEAL")) %>%
    dplyr::mutate(comment = stringr::str_remove(comment, "#REVEAL\\d+")) %>%
    dplyr::mutate(comment = stringr::str_remove(comment, "#REVEAL")) %>%
    dplyr::mutate(connector = stringr::str_extract(stringr::str_trim(code), "%>%$|\\+$|->$|%\\+%")) %>%
    dplyr::mutate(connector = tidyr::replace_na(connector, "")) %>%
    dplyr::mutate(code = stringr::str_remove(stringi::stri_trim_right(code), "%>%$|\\+$|->$|%\\+%")) %>%
    dplyr::mutate(balanced_paren = (cumsum(num_open_par) - cumsum(num_closed_par)) == 0) %>%
    dplyr::mutate(balanced_curly = (cumsum(num_open_curly) - cumsum(num_closed_curly)) == 0) %>%
    dplyr::mutate(balanced_square = (cumsum(num_open_square) - cumsum(num_closed_square)) == 0) %>%
    dplyr::mutate(balanced_par = balanced_paren & balanced_curly & balanced_square &
             code != "") %>%
    dplyr::select(-num_open_par, -num_closed_par,
                  -num_open_curly, -num_closed_curly,
                  -num_open_square, -num_closed_square,
                  -balanced_paren, -balanced_curly, -balanced_square
                  )
    # dplyr::mutate(balanced_par = (cumsum(num_open_par) - cumsum(num_closed_par)) == 0 &
    #          code != "")

}




#' Determine breaks
#'
#' @param parsed parse code dataframe
#' @param break_type text input "auto", "user", or "non_seq"
#'
#' @return a vector of integers
#' @export
#'
#' @examples
#' determine_breaks(parsed = parse_code(local_code), break_type = "user")
determine_breaks <- function(parsed, break_type = "auto"){

  if (break_type == "auto") {

    breaks <- parsed$line[parsed$balanced_par]

    show_order <- cumsum(parsed$balanced_par) + 1 - parsed$balanced_par



  } else if (break_type == "user") {

    breaks <- parsed$line[parsed$user_reveal]

    show_order <-  cumsum(parsed$user_reveal) + 1 - parsed$user_reveal

  } else {

    # make flexible by allowing non integers here.
    show_order <- parsed$user_non_seq

  }

  breaks

}

#' calc_code_panes(parsed = parse_code(local_code), break_type = "user")
calc_code_panes <- function(show_order = c(2, 1, 1, 2, 2, 2, 3)){

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

  } else if (break_type == "multiverse") {

    num_panes <- 10

  }

panes <- list()

  for (i in 1:num_panes) {

    # fix this for non_sequential to allow removal
    panes[[i]] <- which(code_order <= i)

  }

  panes

}



# calc_lines_to_show <- function(parsed = parse_code(local_code_non_sequential))

calc_lines_to_show <- function(breaks){

  parsed %>%
    dplyr::pull(user_non_seq) ->
    code_ordering

  code_ordering %>%
    abs() %>%
    unique() %>%
    sort() ->
    steps

  which_show <- list()

  for (i in steps) {

    which_show[[i]] <- which(
      ifelse(code_ordering > 0,
             code_ordering <= i,
             abs(code_ordering) > i)
    )

  }


  which_show

}

calc_lines_to_highlight <- function(which_show = list(c(1,2), c(1,2,3,4))){


which_highlight <- list()

which_highlight[[1]] <- which_show[[1]]

for (i in 2:length(which_show)) {

  which_highlight[[i]] <- which_show[[i]][!(which_show[[i]] %in% which_show[[i - 1]])]

}



}


#' Calculate highlight for sequential reveal
#'
#' @param breaks a vector of the lines where code breaks are needed
#'
#' @return list of vectors with highlight at each step
#' @export
#'
#' @examples
#' calc_highlight(c(1,5,7,10))
calc_highlight <- function(breaks) {

  highlight <- list()

  for (i in 1:length(breaks)) {
    if (i == 1) {
      highlight[[i]] <- 1:breaks[i]
    } else {
      highlight[[i]] <- (breaks[i - 1] + 1):breaks[i]
    }
  }

  return(highlight)

}





#'
#' #' Parse chunk
#' #'
#' #' @param chunk_name a character string which is a chunk name
#' #'
#' #' @return parsed code from a code chunk in an Rmd
#' #' @export
#' #'
#' #' @examples
#' parse_chunk <- function(chunk_name){
#'
#'   code <- chunk_as_text(chunk_name)
#'
#'   parse_code(code)
#'
#' }


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
reveal_parsed_classic <- function(parsed, break_point = 3, highlight = 1:3){

  parsed %>%
    dplyr::mutate(reveal = 1:dplyr::n() <= break_point) %>%
    dplyr::filter(reveal) %>%
    dplyr::mutate(connector = dplyr::case_when(1:dplyr::n() == dplyr::n() ~ "",
                                 1:dplyr::n() != dplyr::n() ~ connector)) %>%
    dplyr::mutate(highlight = ifelse(1:dplyr::n() %in% highlight, "#<<", "" )) %>%
    dplyr::mutate(out = paste0(code, "", connector, "  ", comment, highlight)) %>%
    dplyr::select(out) ->
    up_to_result

  up_to_result$out

}



#' Partially reveal parsed regular assignment
#'
#' @param parsed the output resulting from parsing code
#' @param break_point an integer indicating the line of code
#' @param highlight integers indicating which lines of code to highlight
#'
#' @return Partial code with indicators for highlight
#' @export
#'
#' @examples
#' reveal_code(code = local_code_regular_assignment)
#' reveal_code(code = local_code)
#' reveal_code(code = local_code, 5, 3:5)
reveal_parsed_reg_assignment <- function(parsed, break_point = 3, highlight = 1:3){

  the_reveal <- reveal_parsed(parsed, break_point, highlight)

  the_reveal[1] %>%
      stringr::str_extract(".+\\<-") %>%
      stringr::str_remove("<-") %>%
      stringr::str_trim() ->
    object_to_track

    c(the_reveal, " ", paste(object_to_track, "# print object"))

}


reveal_parsed <- function(parsed, break_point = 3, highlight = 1:3, reg_assignment = F){

  if (reg_assignment == F) {
    reveal_parsed_classic(parsed, break_point, highlight)
  }else{
      reveal_parsed_reg_assignment(parsed, break_point, highlight)
    }

}

#' #' Reveal Chunk
#' #'
#' #' @param chunk_name a character string which is a chunk name
#' #' @param break_point an integer indicating the line of code
#' #' @param highlight integers indicating which lines of code to highlight
#' #' @param reg_assignment logical set to T if output of some object created at beginning of code chunk should be displayed
#' #'
#' #' @return Partial code with indicators for highlight
#' #' @export
#' #'
#' #' @examples
#' reveal_chunk <- function(chunk_name, break_point = 3, highlight = 1:3, reg_assignment = F){
#'
#'   chunk_name %>%
#'     chunk_as_text() %>%
#'     parse_code() ->
#'   parsed
#'
#'   if (reg_assignment == F) {
#'
#'     reveal_parsed(parsed = parsed, break_point = break_point, highlight = highlight)
#'
#'     }else{
#'
#'     the_reveal <- reveal_parsed(parsed = parsed, break_point = break_point, highlight = highlight)
#'
#'     object_to_track <- the_reveal[1] %>%
#'       stringr::str_extract(".+\\<-") %>%
#'       stringr::str_remove("<-") %>%
#'       stringr::str_trim()
#'
#'     c(the_reveal, " ",
#'       paste(object_to_track, "# print object"))
#'   }
#'
#'
#' }


reveal_chunk <- function(chunk_name, break_point = 3, highlight = 1:3, reg_assignment = F){

  chunk_name %>%
    chunk_as_text() %>%
    parse_code() %>%
    reveal_parsed(break_point, highlight, reg_assignment)

}


return_partial_chunks_template <- function(display_type = "output",
                                  eval = display_type == "output",
                                  echo = display_type == "code") {
  glue::glue("```{r {{{display_type}}}_{{chunk_name}}_{{breaks}}, eval={{{eval}}}, echo = {{{echo}}}, code=reveal_chunk('{{chunk_name}}', break_point = {{breaks}}, highlight = {{highlight}}, reg_assignment = {{reg_assignment}})}",
             "```",
             .open = "{{{", .close = "}}}", .sep = "\n")
}
return_partial_chunks_template()


# return_partial_code_chunks <- function(){
#
#   return_partial_chunks(eval = FALSE, echo = TRUE, display_type = "code")
#
# }
# return_partial_code_chunks()
#
# return_partial_plot_chunks <- function() {
#
#   return_partial_chunks(eval = TRUE, echo = FALSE, display_type = "plot")
#
# }
# return_partial_plot_chunks()



return_partial_side_by_side_code_output_chunks <- function(chunk_name = "a_chunk_name",
                                           breaks = 1:3,
                                           highlight = list(1, 1:2, 1:3),
                                           title = "My Title",
                                           reg_assignment = F,
                                           split = 40) {

  partial_knit_steps <- glue::glue(
    "class: split-{{split}}",
    "count: false",
    "{{title}}",
    ".column[.content[",
    return_partial_chunks_template(display_type = "code"),
    "]]",
    ".column[.content[",
    return_partial_chunks_template(display_type = "output"),
    "]]",
    " ",
    .open = "{{", .close = "}}", .sep = "\n"
    )

  glue::glue_collapse(x = partial_knit_steps, sep = "\n---\n")

}
return_partial_side_by_side_code_output_chunks()



return_partial_code_or_output_chunks <- function(chunk_name = "a_chunk_name",
                                               breaks = 1:3,
                                               highlight = list(1, 1:2, 1:3),
                                               title = "My Title",
                                               reg_assignment = F,
                                               display_type = "output") {

  partial_knit_steps <- glue::glue(
    "count: false",
    "{{title}}",
    return_partial_chunks_template(eval = display_type == "output", echo = display_type == "code", display_type = display_type),
    .open = "{{", .close = "}}", .sep = "\n"
  )

  glue::glue_collapse(x = partial_knit_steps, sep = "\n---\n")

}
return_partial_code_or_output_chunks()



# uses above code, but calculates breaks and highlight
partially_knit_chunks <- function(chunk_name = "example_chunk_name",
                                  title = "My Title",
                                  reg_assignment = F,
                                  display_type = "both",
                                  break_type = "auto",
                                  split = 40){


  chunk_name %>%
    chunk_as_text() %>%
    parse_code() ->
  parsed

breaks <- determine_breaks(parsed, break_type)

highlight <- calc_highlight(breaks = breaks)

if (display_type == "both") {

  return_partial_side_by_side_code_output_chunks(chunk_name,
                                                 title,
                                                 breaks = breaks,
                                                 highlight = highlight,
                                                 reg_assignment,
                                                 split = 40)

} else {

  return_partial_code_or_output_chunks(chunk_name,
                                       title,
                                       breaks = breaks,
                                       highlight = highlight,
                                       reg_assignment)

}

}


#' Apply reveal in Rmarkdown file, to be used in line
#'
#' @param chunk_name a character string which is a chunk name
#' @param user_reveal a logical for if breaks should be automatically determined or have been defined manually with "#REVEAL" message
#' @param show_code a logical for if the code should be displayed or not, default is TRUE
#' @param title a character string for a title for all the slides to display code-output evolution, default is an empty string
#' @param reg_assignment logical set to T if output of some object created at beginning of code chunk should be displayed
#'
#' @return a character string to be interpreted as .Rmd content
#' @export
#'
#' @examples
apply_reveal <- function(chunk_name, display_type = "both", break_type = "auto", title = "", reg_assignment = F, split = 40){

  paste(knitr::knit(text =
                      partially_knit_chunks(chunk_name,
                                            title,
                                            reg_assignment,
                                            display_type,
                                            break_type,
                                            split)),
        collapse = "\n")
}


# partial knit chunks
#'
#'
#' #' Create text that will appear in Rmarkdown document containing code reconstruction chunks
#' #'
#' #' @param chunk_name a character string which is a chunk name
#' #' @param user_reveal a logical for if breaks should be automatically determined or have been defined manually with "#REVEAL" message
#' #' @param show_code a logical for if the code should be displayed or not, default is TRUE
#' #' @param title a character string for a title for all the slides to display code-output evolution, default is an empty string
#' #' @param reg_assignment logical set to T if output of some object created at beginning of code chunk should be displayed
#' #'
#' #' @return a character string to be interpreted as .Rmd content
#' #' @export
#' #'
#' #' @examples
#' partially_knit_chunks <- function(chunk_name = "example_chunk_name", user_reveal = F, show_code = T, title = "", reg_assignment = F) {
#'   # Create slide for lines 1:N for each line N in the given chunk
#'
#'   parsed <- parse_chunk(chunk_name)
#'
#'   if (user_reveal == T) {
#'
#'     breaks <- parsed$line[parsed$user_reveal]
#'
#'   } else {
#'
#'     breaks <- parsed$line[parsed$balanced_par]
#'
#'     }
#'
#'   highlight <- calc_highlight(breaks = breaks)
#'
#'   if (show_code == T) {
#'     partial_knit_steps <- glue::glue(
#'       "class: split-40",
#'       "count: false",
#'       ".column[.content[",
#'       "```{r plot_{{chunk_name}}_{{breaks}}, eval=FALSE, code=reveal_chunk('{{chunk_name}}', {{breaks}}, {{highlight}}, {{reg_assignment}})}",
#'       "```",
#'       "]]",
#'       ".column[.content[",
#'       "```{r output_{{chunk_name}}_{{breaks}}, echo=FALSE, code=reveal_chunk('{{chunk_name}}', {{breaks}}, {{highlight}}, {{reg_assignment}})}",
#'       "```",
#'       "]]",
#'       .open = "{{", .close = "}}", .sep = "\n"
#'     )
#'
#'   } else {
#'
#'     partial_knit_steps <- glue::glue(title,"```{r output_{{chunk_name}}_{{breaks}}, echo=FALSE, code=reveal_chunk('{{chunk_name}}', {{breaks}}, {{highlight}}, {{reg_assignment}})}",
#'                                      "```",
#'                                      .open = "{{", .close = "}}", .sep = "\n"
#'     )
#'
#'   }
#'
#'   glue::glue_collapse(x = partial_knit_steps, sep = "\n---\n")
#'
#' }




