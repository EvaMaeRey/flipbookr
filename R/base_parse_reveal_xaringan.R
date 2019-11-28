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
color = \"blue\", #REVEAL4
) +
aes(color = speed_14_plus)"


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
#' parse_code(code = local_code_regular_assignment)
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
    dplyr::mutate(user_non_seq = stringr::str_extract(comment, "#REVEAL\\d+")) %>%
    dplyr::mutate(user_non_seq = stringr::str_extract(user_non_seq, "\\d+")) %>%
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




#' Parse chunk
#'
#' @param chunk_name a character string which is a chunk name
#'
#' @return parsed code from a code chunk in an Rmd
#' @export
#'
#' @examples
parse_chunk <- function(chunk_name){

  code <- chunk_as_text(chunk_name)

  parse_code(code)

}


#' Partially reveal parsed code
#'
#' @param parsed the output resulting from parsing code
#' @param upto a integer indicating the line of code
#' @param highlight integers indicating which lines of code to highlight
#'
#' @return Partial code with indicators for highlighting
#' @export
#'
#' @examples
reveal_parsed <- function(parsed, upto = 3, highlight = 1:3){

  parsed %>%
    dplyr::mutate(reveal = 1:dplyr::n() <= upto) %>%
    dplyr::filter(reveal) %>%
    dplyr::mutate(connector = dplyr::case_when(1:dplyr::n() == dplyr::n() ~ "",
                                 1:dplyr::n() != dplyr::n() ~ connector)) %>%
    dplyr::mutate(highlight = ifelse(1:dplyr::n() %in% highlight, "#<<", ""
    )) %>%
    dplyr::mutate(out = paste0(code, "", connector, "  ", comment, highlight)) %>%
    dplyr::select(out) ->
    up_to_result
  up_to_result$out

}



#' Partially reveal code
#'
#' @param code code as a character string
#' @param upto an integer indicating the line of code
#' @param highlight integers indicating which lines of code to highlight
#'
#' @return Partial code with indicators for highlighting
#' @export
#'
#' @examples
#' reveal_code(code = local_code_regular_assignment)
#' reveal_code(code = local_code)
#' reveal_code(code = local_code, 5, 3:5)
reveal_code <- function(code, upto = 3, highlight = 1:3) {

  parsed <- parse_code(code = code)

  reveal_parsed(parsed = parsed, upto = upto, highlight = highlight)

}


#' Reveal Chunk
#'
#' @param chunk_name a character string which is a chunk name
#' @param upto an integer indicating the line of code
#' @param highlight integers indicating which lines of code to highlight
#' @param reg_assignment logical set to T if output of some object created at beginning of code chunk should be displayed
#'
#' @return Partial code with indicators for highlighting
#' @export
#'
#' @examples
reveal_chunk <- function(chunk_name, upto = 3, highlight = 1:3, reg_assignment = F){

  content <- chunk_as_text(chunk_name)
  parsed <- parse_code(code = content)

  if (reg_assignment == F) {
    reveal_parsed(parsed = parsed, upto = upto, highlight = highlight)
  }else{
    the_reveal <- reveal_parsed(parsed = parsed, upto = upto, highlight = highlight)

    object_to_track <- the_reveal[1] %>%
      stringr::str_extract(".+\\<-") %>%
      stringr::str_remove("<-") %>%
      stringr::str_trim()

    c(the_reveal, " ",
      paste(object_to_track, "# print object"))
  }


}


#' Calculate highlighting for sequential reveal
#'
#' @param breaks a vector of the lines where code breaks are needed
#'
#' @return list of vectors with highlighting at each step
#' @export
#'
#' @examples
#' calc_highlight(c(1,5,7,10))
calc_highlight <- function(breaks) {

  highlighting <- list()

for (i in 1:length(breaks)) {
  if (i == 1) {
    highlighting[[i]] <- 1:breaks[i]
  } else {
    highlighting[[i]] <- (breaks[i - 1] + 1):breaks[i]
  }
}

  return(highlighting)

}




return_partial_chunks <- function(type = "output",
                                  eval = type == "output",
                                  echo = type == "code") {
  glue::glue("```{r {{{type}}}_{{chunk_name}}_{{breaks}}, eval={{{eval}}}, echo = {{{echo}}}, code=reveal_chunk('{{chunk_name}}', {{breaks}}, {{highlighting}}, {{reg_assignment}})}",
             "```",
             .open = "{{{", .close = "}}}", .sep = "\n")
}
return_partial_chunks()


# return_partial_code_chunks <- function(){
#
#   return_partial_chunks(eval = FALSE, echo = TRUE, type = "code")
#
# }
# return_partial_code_chunks()
#
# return_partial_plot_chunks <- function() {
#
#   return_partial_chunks(eval = TRUE, echo = FALSE, type = "plot")
#
# }
# return_partial_plot_chunks()



return_partial_side_by_side_code_output_chunks <- function(chunk_name = "a_chunk_name",
                                           breaks = 1:3,
                                           highlighting = list(1, 1:2, 1:3),
                                           title = "My Title",
                                           reg_assignment = F,
                                           split = 40) {

  partial_knit_steps <- glue::glue(
    "class: split-{{split}}",
    "count: false",
    "{{title}}",
    ".column[.content[",
    return_partial_chunks(type = "code"),
    "]]",
    ".column[.content[",
    return_partial_chunks(type = "output"),
    "]]",
    .open = "{{", .close = "}}", .sep = "\n"
    )

  glue::glue_collapse(x = partial_knit_steps, sep = "\n---\n")

}
return_partial_side_by_side_code_output_chunks()



return_partial_code_or_output_chunks <- function(chunk_name = "a_chunk_name",
                                               breaks = 1:3,
                                               highlighting = list(1, 1:2, 1:3),
                                               title = "My Title",
                                               reg_assignment = F,
                                               type = "output") {

  partial_knit_steps <- glue::glue(
    "count: false",
    "{{title}}",
    return_partial_chunks(eval = type == "output", echo = type == "code", type = type),
    .open = "{{", .close = "}}", .sep = "\n"
  )

  glue::glue_collapse(x = partial_knit_steps, sep = "\n---\n")

}
return_partial_code_or_output_chunks()



# uses above code, but calculates breaks and highlighting
partially_knit_chunks <- function(chunk_name = "example_chunk_name",
                                  title = "My Title",
                                  reg_assignment = F,
                                  type = NULL,
                                  user_reveal = F){


parsed <- parse_chunk(chunk_name)

if (user_reveal == T) {

    breaks <- parsed$line[parsed$user_reveal]

  } else {

    breaks <- parsed$line[parsed$balanced_par]

  }

highlighting <- calc_highlight(breaks = breaks)

if (is.null(type)) {

  return_partial_side_by_side_code_output_chunks()

} else {

  return_partial_code_or_output_chunks()

}

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
#'   highlighting <- calc_highlight(breaks = breaks)
#'
#'   if (show_code == T) {
#'     partial_knit_steps <- glue::glue(
#'       "class: split-40",
#'       "count: false",
#'       ".column[.content[",
#'       "```{r plot_{{chunk_name}}_{{breaks}}, eval=FALSE, code=reveal_chunk('{{chunk_name}}', {{breaks}}, {{highlighting}}, {{reg_assignment}})}",
#'       "```",
#'       "]]",
#'       ".column[.content[",
#'       "```{r output_{{chunk_name}}_{{breaks}}, echo=FALSE, code=reveal_chunk('{{chunk_name}}', {{breaks}}, {{highlighting}}, {{reg_assignment}})}",
#'       "```",
#'       "]]",
#'       .open = "{{", .close = "}}", .sep = "\n"
#'     )
#'
#'   } else {
#'
#'     partial_knit_steps <- glue::glue(title,"```{r output_{{chunk_name}}_{{breaks}}, echo=FALSE, code=reveal_chunk('{{chunk_name}}', {{breaks}}, {{highlighting}}, {{reg_assignment}})}",
#'                                      "```",
#'                                      .open = "{{", .close = "}}", .sep = "\n"
#'     )
#'
#'   }
#'
#'   glue::glue_collapse(x = partial_knit_steps, sep = "\n---\n")
#'
#' }



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
apply_reveal <- function(chunk_name, user_reveal = F, type = NULL, title = "", reg_assignment = F){

  paste(knitr::knit(text =
                      partially_knit_chunks(chunk_name,
                                            user_reveal,
                                            type,
                                            title,
                                            reg_assignment)),
        collapse = "\n")
}

