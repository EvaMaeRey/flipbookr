# Emi Tanaka (@statsgen) and Garrick Aden-Buie (@grrrck) and Evangeline Reynolds (@EvaMaeRey)
# have contributed to this code

####### Make some test code available as character strings #####
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
  my_plot  #BREAK


  1 + 1 #BREAK"

}



####### Make some test code available as character strings #####
create_rotate_code <- function(){ # for testing w/o knitting

  "cars %>%             # the data  #BREAK
  filter(speed > 4) %>%  # subset
  ggplot() +              # pipe to ggplot
  aes(x = speed) +
  aes(y = dist) +
  # Describing what follows
  geom_point(alpha = .3) + #ROTATE
  geom_point(color = 'blue') + #ROTATE
  geom_point(shape = 'square') -> #ROTATE
  my_plot  #BREAK


  1 + 1 #BREAK"

}


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


create_python_code <- function(){

  "xobject = load_iris()
xobject = pd.DataFrame(xobject.data,
columns=xobject.feature_names)
def evenOdd( x ):
    if (x % 2 == 0):
        print \"even\"
    else:
        print \"odd\"

# Driver code
evenOdd(2)
xobject.pipe(remove_units).pipe(length_times_width)"

}

create_data_table_code <- function(){ # for testing w/o knitting

  'gapminder::gapminder %>%
  data.table() %>%
   .[year > 1980] %>%
   .[                       ,
     mean(gdpPercap)        ,
     by = .(continent, year) ]'

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



####### Get code from source chunk #####
chunk_code_get <- function(chunk_name){

  paste(knitr::knit_code$get(chunk_name), collapse = "\n")

}

# create_code() %>%
#   code_as_table() %>%
#   code_as_table_process_break_messages()


#### Code parsing #########
code_as_table <- function(code){

  code %>%
    stringr::str_split(pattern = "\n") %>%
    .[[1]] %>%
    tibble::tibble(raw_code = .) %>%
    dplyr::mutate(line = 1:dplyr::n())

}

code_as_table_process_break_messages <- function(code_as_table){

  code_as_table %>%
    dplyr::mutate(raw_code = stringr::str_remove(raw_code, "\\s+$")) %>%
    dplyr::mutate(non_seq = stringr::str_extract(raw_code, "#BREAK-?\\d+")) %>%
    dplyr::mutate(non_seq = stringr::str_extract(non_seq, "-?\\d+")) %>%
    dplyr::mutate(non_seq = as.numeric(non_seq)) %>%
    dplyr::mutate(non_seq = tidyr::replace_na(non_seq, 1)) %>%
    dplyr::mutate(user = stringr::str_detect(raw_code, "#BREAK$")) %>%
    dplyr::mutate(rotate = stringr::str_detect(raw_code, "#ROTATE$"))

}


code_simple_parse <- function(code){

  code %>%
    code_as_table() %>%
    code_as_table_process_break_messages()

}

# dplyr::mutate(comment = stringr::str_remove(comment, "#BREAK\\d+")) %>%


# create_code() %>%
#   code_as_table() %>%
#   code_as_table_process_break_messages()


#### Real Parsing ####


r_code_base_parse <- function(code) {

  sf <- srcfile(code)
  try(parse(text = code, srcfile = sf))
  utils::getParseData(sf)

}


r_base_parsed_count_parentheses <- function(base_parsed){


  num_lines <- max(base_parsed$line1)

  tibble::tibble(line = 1:num_lines) ->
  all_lines

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
    dplyr::full_join(all_lines) %>%
    dplyr::arrange(line) %>%
    dplyr::mutate(
      full_line = tidyr::replace_na(full_line, ""),
      comment = tidyr::replace_na(comment, ""),
      num_open_par = tidyr::replace_na(num_open_par, 0),
      num_closed_par = tidyr::replace_na(num_closed_par, 0),
      num_open_curly = tidyr::replace_na(num_open_curly, 0),
      num_closed_curly = tidyr::replace_na(num_closed_curly, 0),
      num_open_square = tidyr::replace_na(num_open_square, 0),
      num_closed_square = tidyr::replace_na(num_closed_square, 0)
    ) %>%
    dplyr::mutate(balanced_paren = (cumsum(num_open_par) - cumsum(num_closed_par)) == 0) %>%
    dplyr::mutate(balanced_curly = (cumsum(num_open_curly) - cumsum(num_closed_curly)) == 0) %>%
    dplyr::mutate(balanced_square = (cumsum(num_open_square) - cumsum(num_closed_square)) == 0) %>%
    dplyr::mutate(all_parentheses_balanced = balanced_paren & balanced_curly & balanced_square) %>%
    dplyr::select(line, full_line, comment, all_parentheses_balanced)


}


# create_code() %>%
#   r_code_base_parse() %>%
#   r_base_parsed_count_parentheses()


#### Full parse R, python, stata ####


r_code_full_parse <- function(code = code){

  arithmetic <- "\\+$|-$|\\/$|\\*$|\\^$|%%$|%\\/%$"
  matrix <- "%\\*%$|%o%$"
  ggplot_change_data <- "%\\+%$"
  the_magrittr <- "%>%$|%\\$%$"
  right_assign <- "->$"
  combine_booleans <- "\\|$|\\&$"

  connectors <- paste(arithmetic, matrix, ggplot_change_data,
                       the_magrittr,
                       right_assign, combine_booleans, sep = "|")

  raw_code_table <- code_simple_parse(code = code)

  parsed_code_table <- code %>%
    r_code_base_parse() %>%
    r_base_parsed_count_parentheses()

  raw_code_table %>%
    dplyr::full_join(parsed_code_table) %>%
    # we need this XXXXXXX so that we don't get a bunch of warnings
    dplyr::mutate(comment = tidyr::replace_na(comment, "XXXXXXXXX")) %>%
    dplyr::mutate(comment = stringr::str_replace(comment, "^$", "XXXXXXXXX")) %>%
    dplyr::mutate(code = stringr::str_remove(raw_code, comment)) %>%
    dplyr::mutate(connector = stringr::str_extract(stringr::str_trim(code), connectors)) %>%
    dplyr::mutate(connector = tidyr::replace_na(connector, "")) %>%
    # delete comments understood as
    dplyr::mutate(comment = stringr::str_remove(comment, "#BREAK\\d?")) %>%
    dplyr::mutate(comment = stringr::str_remove(comment, "#ROTATE")) %>%
    dplyr::mutate(comment = stringr::str_remove(comment, "XXXXXXXXX")) %>%
    dplyr::mutate(code = stringr::str_remove(stringi::stri_trim_right(code), connectors))  %>%
    dplyr::mutate(auto = all_parentheses_balanced & code != "") %>%
    dplyr::select(line, raw_code, code, connector, comment, auto, user, non_seq, rotate)

}


# create_python_code() %>%
#   python_code_full_parse()


python_code_full_parse <- function(code){

  code %>%
    code_simple_parse() %>%
    dplyr::mutate(code = raw_code) %>%
    dplyr::mutate(open_par = stringr::str_count(code, "\\{|\\(|\\[")) %>%
    dplyr::mutate(closed_par = stringr::str_count(code, "\\}|\\)|\\]")) %>%
    dplyr::mutate(auto = cumsum(open_par) == cumsum(closed_par)) %>%
    dplyr::mutate(auto = ifelse(raw_code == "", FALSE, auto)) %>%
    dplyr::mutate(indented = stringr::str_detect(code, "^\\s+")) %>%
    dplyr::mutate(indented_follows = dplyr::lead(indented, default = FALSE)) %>%
    dplyr::mutate(auto = ifelse(indented_follows, FALSE, auto)) %>%
    dplyr::mutate(connector = "") %>%
    dplyr::mutate(comment = "")

}


stata_code_full_parse <- function(code){

  code %>%
    code_simple_parse() %>%
    dplyr::mutate(code = raw_code) %>%
    dplyr::mutate(auto = ifelse(raw_code == "", FALSE, TRUE)) %>%
    dplyr::mutate(connector = "") %>%
    dplyr::mutate(comment = "")

}


#### Combined code parsing all languages ####

code_parse <- function(code = create_code(), lang = "r") {

  if (lang == "r") {

    r_code_full_parse(code = code) %>%
      dplyr::mutate(func = stringr::str_extract(code, "\\w+\\(")) %>%
      dplyr::mutate(func = stringr::str_remove(func, "\\("))

  } else if (lang == "python") {

  python_code_full_parse(code = code)

  } else if (lang == "stata") {

  NULL

  }




}

# create_rotate_code() %>%
#   code_parse() %>%
#   parsed_calc_show(break_type = "rotate") %>%
#   shown_lines_calc_highlight()


#### Calculate lines and highlighting to show in frames ####
parsed_calc_show <- function(parsed, break_type = "auto"){

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

  } else if (break_type == "rotate") {

    num_panes <- sum(parsed$rotate)

  }



  which_show <- list()

  if (break_type == "rotate") {

    for (i in 1:num_panes) {

      which_show[[i]] <-
        sort(c(which(!parsed$rotate),
          which(parsed$rotate)[i]))
    }

  } else {

    for (i in 1:num_panes) {

      # fix this for non_sequential to allow removal
      which_show[[i]] <- which(code_order <= i)
      # Matt Gambino: change pipes to second statement to drop negative values
      # which_show[[i]] <-
      #   which( code_order <= i ) %>%
      #   .[!. %in% which( code_order >= -i & code_order < 0 )]

    }

  }

  which_show

}




shown_lines_calc_highlight <- function(which_show = list(c(1, 2), c(1, 2, 3, 4)),
                                    break_type = "auto"){

  which_highlight <- list()


  # first frame highlighting

  if (break_type == "user" | break_type == "auto") {

  which_highlight[[1]] <- which_show[[1]]

  }

  if (break_type == "non_seq" | break_type == "rotate") {

    which_highlight[[1]] <- as.integer(c())

  }

  # additional frames highlighting

  if (break_type %in% c("user", "auto", "non_seq", "rotate")) {

    for (i in 2:length(which_show)) {

      which_highlight[[i]] <- which_show[[i]][!(which_show[[i]] %in% which_show[[i - 1]])]

    }

  }

  # multiverse highlighting

  if (is.numeric(break_type)) {

      for (i in 1:length(which_show)) {

        which_highlight[[i]] <- as.integer(c())

      }

  }

  which_highlight

}



# create_code() %>%
  # code_parse() %>%
#   parsed_return_partial_code(which_show_frame = 1:5,
#                              which_highlight_frame = 4)


#### Return partial code builds for frames ######
parsed_return_partial_code <- function(parsed,
                                       which_show_frame = 1:3,
                                       which_highlight_frame = 3){

  parsed %>%
    dplyr::filter(line %in% which_show_frame) %>%
    dplyr::mutate(connector = dplyr::case_when(1:dplyr::n() == dplyr::n() ~ "",
                                               1:dplyr::n() != dplyr::n() ~ connector)) %>%
    dplyr::mutate(highlight = ifelse(line %in% which_highlight_frame, "#<<", "" )) %>%
    dplyr::mutate(highlight = ifelse(code == "" | code == "\\s?", "", highlight)) %>%
    dplyr::mutate(out = paste0(code, "",
                               connector,
                               ifelse(code == "", "", "  "),
                               comment, highlight)) %>%
    dplyr::pull()

}



parsed_return_recent_function <- function(parsed,
                                          which_highlight_frame = 3){

  parsed %>%
    dplyr::filter(line %in% which_highlight_frame) %>%
    dplyr::pull(func)

}


# create_code() %>%
#   code_parse() %>%
#   parsed_return_recent_function()

parsed_left_assign_return_partial_code <- function(parsed,
                                               which_show_frame = 1:3,
                                               which_highlight_frame = 3){

  the_reveal <- parsed_return_partial_code(parsed,
                                                which_show_frame,
                                                which_highlight_frame)

  the_reveal[1] %>%
      stringr::str_extract(".+\\<-|.+\\=") %>%
      stringr::str_remove("<-|=") %>%
      stringr::str_trim() ->
    object_to_track  # this is the object created at the beginning of the code chunk

    c(the_reveal, " ", object_to_track)

}



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


parsed_return_recent_function_sequence <- function(parsed,
                                                   break_type = "auto",
                                                   which_show = parsed_calc_show(parsed = parsed,
                                                                                  break_type = break_type),
                                                   which_highlight =
                                                              shown_lines_calc_highlight(which_show = which_show,
                                                                                         break_type = break_type),
                                                            left_assign = F){

  partial_recent_functions <- list()

  for (i in 1:length(which_show)) {


    if (left_assign == F) {
      partial_recent_functions[[i]] <-
        parsed_return_recent_function(parsed,
                                   which_highlight_frame = which_highlight[[i]]) %>% .[!is.na(.)]
    }else{
      partial_recent_functions[[i]] <-
        parsed_return_recent_function(parsed,
                                      which_highlight_frame = which_highlight[[i]]) %>% .[!is.na(.)]
    }

  }

  partial_recent_functions


}


# create_code() %>%
#   code_parse() %>%
#   parsed_return_recent_function_sequence()


chunk_name_return_code_sequence <- function(chunk_name,
                                            break_type = "auto",
                                            left_assign = F,
                                            lang = "r"){

chunk_name %>%
  chunk_code_get() %>%
  code_parse(lang = lang) %>%
  parsed_return_partial_code_sequence(break_type = break_type,
                                      left_assign = left_assign)

}

chunk_name_return_function_sequence <- function(chunk_name,
                                            break_type = "auto",
                                            left_assign = F,
                                            lang = "r"){

  chunk_name %>%
    chunk_code_get() %>%
    code_parse(lang = lang) %>%
    parsed_return_recent_function_sequence(break_type = break_type,
                                            left_assign = left_assign)

}


#### Template code chunks to deliver partial builds on ####
return_partial_chunks_template_code <- function(){

  "```{<<<lang>>> <<<chunk_name>>>_<<<break_type>>>_<<<breaks>>>_code, eval = FALSE, echo = TRUE, code = code_seq[[<<<breaks>>>]]}
```"

}

return_partial_chunks_template_output <- function(){

  "```{<<<lang>>> <<<chunk_name>>>_<<<break_type>>>_<<<breaks>>>_output, eval = TRUE, echo = FALSE, code = code_seq[[<<<breaks>>>]]}
```"

  # , out.width = \"<<<out.width>>>\", out.height = \"<<<out.height>>>\"
}


return_partial_chunks_template_function <- function(){

  "```{<<<lang>>> <<<chunk_name>>>_<<<break_type>>>_<<<breaks>>>_function, eval = TRUE, echo = FALSE, code = func_seq[[<<<breaks>>>]]}
```"

  # , out.width = \"<<<out.width>>>\", out.height = \"<<<out.height>>>\"
}


return_markdown <- function(text, sep = "|"){

  text %>%
    stringr::str_split(pattern = sep)

}


#### define CSS ####


# choose_css_components <- function(display_type = "both"){
#
# left <- ".left-panel-<<<id>>> {
#       color: #777;
#       width: <<<width_left>>>;
#       height: 92%;
#       float: left;
#       font-size: <<<font_size_code>>>
#     }"
#
# right <- ".right-panel-<<<id>>> {
#       width: <<<width_right>>>;
#       float: right;
#       padding-left: 1%;
#     }"
#
# middle <- ".middle-panel-<<<id>>> {
#       width: <<<width_middle>>>;
#       float: center;
#       padding-left: 1%;
#     }"
#
# if (display_type[1] == "both" | length(display_type) == 2) {
#   paste0(left, right)
# } else if (length(display_type) == 2) {
#   paste0(left, right)
# } else if (length(display_type) == 3) {
#   paste0(left, middle, right)
# }
#
# }


define_css <- function(
  chunk_name = "example",
  break_type = "auto",
  widths = c(32, 32, 33),
  spacing = 1,
  font_size_code = 80
){

  normalized_widths <- 100*widths/(sum(widths) + spacing*(length(widths) - 1))

  width_left <-  32
  width_right <- 32
  width_middle <- 32

  try(width_left   <- normalized_widths[1])
  try(width_right  <- normalized_widths[2])
  try(width_middle <- normalized_widths[3])

  id <- paste0(chunk_name, "-", break_type)

  knitr::asis_output(glue::glue(
    "<style>
    .left-panel-<<<id>>> {
      color: #777;
      width: <<<width_left>>>%;
      height: 92%;
      float: left;
      font-size: <<<font_size_code>>>
    }
    .right-panel-<<<id>>> {
      width: <<<width_right>>>%;
      float: right;
      padding-left: 1%;
      font-size: <<<font_size_code>>>
    }
    .middle-panel-<<<id>>> {
      width: <<<width_middle>>>%;
      float: left;
      padding-left: 1%;
      font-size: <<<font_size_code>>>
    }
    </style>
    ",
    .open = "<<<",
    .close = ">>>",
    .sep = "\n"
  ))
}

# define_css()

# return_css()

# create_code() %>%
#   code_parse() %>%
#   parsed_return_partial_code_sequence()

# "code_chunk_name" %>%
#   chunk_expand(lang = "python")

###### Create spauned code chunks ######
chunk_expand <- function(chunk_name = "example",
                         break_type = "auto",
                         display_type = c("code", "output"),
                         num_breaks = 2,
                         title = "",
                         md = NULL,
                         func = NULL,
                         lang = "r",
                         custom = F,
                         widths = c(39, 60, 0),
                         font_size_code = "80%"
){

  breaks <- 1:num_breaks
  code <- return_partial_chunks_template_code()
  output <- return_partial_chunks_template_output()
  md <- "`r md[<<<breaks>>>]`"
  func <- return_partial_chunks_template_function()


if (display_type[1] == "both") {
  left <- code
  right <- output
} else if (length(display_type) == 1) {
  left <- get(display_type)
} else if (length(display_type) == 2) {
  left <- get(display_type[1])
  right <- get(display_type[2])
} else if (length(display_type) == 3) {
  left <- get(display_type[1])
  middle <- get(display_type[2])
  right <- get(display_type[3])
}

  if (length(display_type) == 3) {

    partial_knit_steps <- glue::glue(
      #"class: split-<<<split>>>",
      "count: false",
      " ",
      title,
      ".left-panel-<<<chunk_name>>>-<<<break_type>>>[",
      left,
      "]",
      " ",
      ".middle-panel-<<<chunk_name>>>-<<<break_type>>>[",
      middle,
      "]",
      " ",
      ".right-panel-<<<chunk_name>>>-<<<break_type>>>[",
      right,
      "]",
      " ",
      .open = "<<<", .close = ">>>", .sep = "\n"
    )

  } else if (length(display_type) == 2 | display_type[1] == "both") {

    partial_knit_steps <- glue::glue(
      #"class: split-<<<split>>>",
      "count: false",
      " ",
      title,
      ".left-panel-<<<chunk_name>>>-<<<break_type>>>[",
      left,
      "]",
      " ",
      ".right-panel-<<<chunk_name>>>-<<<break_type>>>[",
      right,
      "]",
      " ",
      .open = "<<<", .close = ">>>", .sep = "\n"
    )

  } else if (length(display_type) == 1) {

    partial_knit_steps <- glue::glue(
      "count: false",
      title,
      left,
      .open = "<<<", .close = ">>>", .sep = "\n"
    )

  }


the_defined_css <- define_css(chunk_name = chunk_name,
                          break_type = break_type,
                          widths = widths,
                          font_size_code = font_size_code
                          )

  slide_code <- glue::glue_collapse(partial_knit_steps, sep = "\n\n---\n")

  glue::glue("{slide_code}\n\n{the_defined_css}\n\n", .trim = FALSE)


}




# chunk_expand()
# defined_css %>%
#   str()
#
# expanded %>%
#   str()
#
# chunk_expand() %>%
#   length()

######## The exported function ############

#' Title
#'
#' @param chunk_name a character string refering to the name of the source chunk for the flipbooking
#' @param break_type "auto" is default finding appropriate breakpoints, "user" can be used with the special comment message #BREAK within the source code chunk, "non_seq" can be used for non sequential display of code with special comment messages #BREAK2 (will show in second frame) and #BREAK3 (will show in third frame), an integer input can be given too, to simply display the source code chunk multiple times which is appropriate for observing multiple realizations of sampling, "rotate" allows cycling through different lines of code, the comment #ROTATE is used for lines to by cycled through
#' @param left_assign a logical, default is FALSE, if TRUE will print the object created in the upper lefthand corner of the source code chunk at the end of each partial reveal
#' @param code_seq a list of code as character strings, the list will automatically be created based on the previous three arguments or the user can input code manually
#' @param num_breaks an integer, automatically calculated based on the length of the the code_seq list
#' @param display_type the default is "both" for code and output to be displayed side-by-side, "output" will create spauned code chunks to only display output, "code" will create spauned code chunks only to show the partial code builds
#'
#' @return a string object - will only work in 'knitr' context
#' @export
#'
chunk_reveal <- function(chunk_name = NULL,
                   break_type = "auto",
                   left_assign = F,
                   lang = "r",
                   code_seq = NULL,
                   func_seq = NULL,
                   num_breaks = NULL,
                   display_type = c("code", "output"),
                   title = "",
                   md = NULL,
                   widths = c(39, 60, 0),
                   font_size_code = "80%"
                   #,
                   # out.width = "70%",
                   # out.height = "70%"
                   ){

  if (!is.null(chunk_name)) {
  code_seq <- chunk_name_return_code_sequence(chunk_name, break_type, left_assign, lang)
  num_breaks <- length(code_seq)
    if (!is.null(func_seq)){
    func_seq <- chunk_name_return_function_sequence(chunk_name, break_type, left_assign, lang)
    }
  }

  text <- chunk_expand(chunk_name = chunk_name,
                       break_type = break_type,
                       num_breaks = num_breaks,
                       display_type = display_type,
                       title = title,
                       lang = lang,
                       md = md,
                       func = func,
                       widths = widths,
                       font_size_code = font_size_code
                       #,
                       #out.height = out.height,
                       #out.width = out.width
                       )

paste(knitr::knit(text = text), collapse = "\n")

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

# chunk_expand_beamer()

# chunk_expand_beamer <- function(chunk_name = "example",
#                                 break_type = "auto",
#                                 display_type = "both",
#                                 num_breaks = 2,
#                                 title = "",
#                                 lang = "r",
#                                 custom = F,
#                                 width_left = "38%",
#                                 width_right = "60%",
#                                 font_size_code = "80%"
# ){
#
#   breaks <- 1:num_breaks
#
#   partial_knit_steps <- glue::glue(
#     # \documentclass{beamer}
#     # \begin{document}
#     "\\begin{frame}",
#     "\\begin{columns}[T] % align columns",
#     "\\begin{column}{.48\\textwidth}",
#     # "\color{red}\rule{\linewidth}{4pt}",
#     # Left Part
#     return_partial_chunks_template_code(),
#     "\\end{column}%",
#     "\\hfill%",
#     "\\begin{column}{.48\\textwidth}",
#
#     # "\color{blue}\rule{\linewidth}{4pt}"
#     # Right Part
#     return_partial_chunks_template_output(),
#     "\\end{column}%",
#     "\\end{columns}",
#     "\\end{frame}",
#     # \end{document}
#     .open = "<<<", .close = ">>>", .sep = "\n"
#   )
#
#
#   partial_knit_steps
#
# }
