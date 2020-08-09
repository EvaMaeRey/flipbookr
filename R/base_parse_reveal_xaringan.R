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

  NULL #OMIT

  1 + 1 #BREAK"

}





# create_injectable_code() %>%
#   code_highlight_inject_vector()

code_highlight_inject_vector <- function(inject_code, injection = 1:3){

  injection <- as.character(injection)
  code_seq <- list()

for (i in 1:length(injection)){

  code_seq[[i]] <- inject_code %>%
    code_as_table() %>%
    dplyr::mutate(code = ifelse(stringr::str_detect(raw_code, "#VECTOR"),
                         paste(raw_code, "#<<"),
                         raw_code)) %>%
    dplyr::mutate(code =
             stringr::str_replace_all(code,
                                      "#VECTOR",
                                      injection[i])) %>%
    dplyr::pull(code)

}

code_seq

}




create_code_rotate_omit <- function(){

'ggplot(data = cars) +
  aes(x = speed) +
  aes(y = dist) +
  geom_point(size = 8,
             shape = 21,
             alpha = .9,
             color = "snow") +
  aes(fill = speed) +
  scale_fill_viridis_c(option = "magma") + #OMIT
  scale_fill_viridis_c(option = "magma") + #ROTATE
  scale_fill_viridis_c(option = "cividis") + #ROTATE
  scale_fill_viridis_c(option = "plasma") + #ROTATE
  NULL'

}




create_injectable_code <- function(){

  "for (i in 1:#VECTOR){
  print(i)
}  "

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

create_python_code_pipeline <- function(){

  "student_scores \\\n  .melt(id_vars=['student', \"sex\"], \n        var_name=\"subject\", \n        value_name=\"final_grade\") \\\n  .sort_values(by=['final_grade'], ascending=False) \\\n  .head(3)"

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



code_remove_omit <- function(code, omit = "#OMIT"){

  code %>%
    stringr::str_split(pattern = "\n") %>%
    .[[1]] %>%
    .[!stringr::str_detect(., omit)] %>%
    paste(collapse = "\n")

}


#
# create_code() %>%
#   code_as_table() %>%
#   code_as_table_process_break_messages()

#### Code parsing #########
code_as_table <- function(code, omit = "#OMIT"){

  code %>%
    code_remove_omit(omit = omit) %>%
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

# create_code() %>%
#   code_as_table() %>%
#   code_as_table_process_break_messages()


code_simple_parse <- function(code, omit = "#OMIT"){

  code %>%
    code_as_table(omit = omit) %>%
    code_as_table_process_break_messages()

}


#### Real Parsing ####


r_code_base_parse <- function(code, omit = "#OMIT") {

  code <- code_remove_omit(code = code, omit = omit)
  # code <- stringr::str_remove_all(code, "#BREAK\\d+|#BREAK|#ROTATE|#OMIT")

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


r_code_full_parse <- function(code = code, omit = "#OMIT"){

  arithmetic <- "\\+$|-$|\\/$|\\*$|\\^$|%%$|%\\/%$"
  matrix <- "%\\*%$|%o%$"
  ggplot_change_data <- "%\\+%$"
  the_magrittr <- "%>%$|%\\$%$"
  right_assign <- "->$"
  combine_booleans <- "\\|$|\\&$"

  connectors <- paste(arithmetic, matrix, ggplot_change_data,
                       the_magrittr,
                       right_assign, combine_booleans, sep = "|")

  raw_code_table <- code_simple_parse(code = code, omit = omit)

  parsed_code_table <- code %>%
    r_code_base_parse(omit = omit) %>%
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
    dplyr::mutate(comment = stringr::str_remove(comment, "#[[A-Z]]+")) %>%
    dplyr::mutate(comment = stringr::str_remove(comment, "XXXXXXXXX")) %>%
    dplyr::mutate(code = stringr::str_remove(stringi::stri_trim_right(code), connectors))  %>%
    dplyr::mutate(auto = all_parentheses_balanced & code != "") %>%
    dplyr::select(line, raw_code, code, connector, comment, auto, user, non_seq, rotate)

}


# create_python_code() %>%
#   python_code_full_parse()

# create_python_code_pipeline() %>%
#   python_code_full_parse()
#
# code <- create_python_code_pipeline()


python_code_full_parse <- function(code, omit = "#OMIT"){

  connectors <- "\\\\"

  code %>%
    code_simple_parse(omit = omit) %>%
    dplyr::mutate(code = raw_code) %>%
    dplyr::mutate(open_par = stringr::str_count(code, "\\{|\\(|\\[")) %>%
    dplyr::mutate(closed_par = stringr::str_count(code, "\\}|\\)|\\]")) %>%
    dplyr::mutate(auto = cumsum(open_par) == cumsum(closed_par)) %>%
    dplyr::mutate(auto = ifelse(raw_code == "", FALSE, auto)) %>%
    dplyr::mutate(indented = stringr::str_detect(code, "^\\s+")) %>%
    # dplyr::mutate(indented_follows = dplyr::lead(indented, default = FALSE)) %>%
    # dplyr::mutate(auto = ifelse(indented_follows, FALSE, auto))  %>%
    dplyr::mutate(connector = stringr::str_extract(stringr::str_trim(code), connectors)) %>%
    dplyr::mutate(connector = tidyr::replace_na(connector, "")) %>%
    # dplyr::mutate(connector = stringr::str_replace(connector, "\\\\", "\\")) %>%
    dplyr::mutate(code = stringr::str_remove(stringi::stri_trim_right(code), connectors))  %>%
    dplyr::mutate(comment = "")

}


stata_code_full_parse <- function(code, omit = "#OMIT"){

  code %>%
    code_simple_parse(omit = omit) %>%
    dplyr::mutate(code = raw_code) %>%
    dplyr::mutate(auto = ifelse(raw_code == "", FALSE, TRUE)) %>%
    dplyr::mutate(connector = "") %>%
    dplyr::mutate(comment = "")

}


#### Combined code parsing all languages ####

code_parse <- function(code = create_code(), lang = "r", omit = "#OMIT") {

  if (lang == "r") {

    r_code_full_parse(code = code, omit = omit) %>%
      dplyr::mutate(func = stringr::str_extract(code, "\\w+\\(")) %>%
      dplyr::mutate(func = stringr::str_remove(func, "\\("))

  } else if (lang %in% c("python", "py")) {

  python_code_full_parse(code = code, omit = omit)

  } else if (lang == "stata") {

  NULL

  }

}

# create_rotate_code() %>%
#   code_parse() %>%
#   parsed_calc_show(break_type = "rotate") %>%
#   shown_lines_calc_highlight()

#
# create_python_code_pipeline() %>%
#   code_parse(lang = "python") %>%
#   parsed_return_partial_code_sequence()


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

  } else if (break_type == "inject_vector") {

    num_panes <- length(injection)

  }



  which_show <- list()

  if (break_type == "rotate") {

    for (i in 1:num_panes) {

      which_show[[i]] <-
        sort(
           c(which(!parsed$rotate),
          which(parsed$rotate)[i]
          ))
    }

  } else if (break_type == "inject_vector")  {
    NULL
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
                                       break_type = "auto",
                                       parsed){

  which_highlight <- list()


  # first frame highlighting

  if (break_type == "user" | break_type == "auto") {

  which_highlight[[1]] <- which_show[[1]]

  }

  if (break_type == "non_seq") {

    which_highlight[[1]] <- as.integer(c())

  }

  if (break_type == "rotate") {

    which_highlight[[1]] <- which_show[[1]][!(which_show[[1]] %in% which_show[[2]])]

  }

  if (break_type == "inject_vector") {

    NULL

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

# create_python_code_pipeline() %>%
#   code_parse()



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
                                 which_highlight_frame = which_highlight[[i]]) %>%
      stringr::str_trim(side = "right") # this is for python
  }else{
    partial_code_frames[[i]] <-
      parsed_left_assign_return_partial_code(parsed,
                                            which_show_frame = which_show[[i]],
                                            which_highlight_frame = which_highlight[[i]]) %>%
      stringr::str_trim(side = "right") # this is for python
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
                                            lang = "r",
                                            omit = "#OMIT"){

if (break_type == "inject_vector"){

  chunk_name %>%
    chunk_code_get() %>%
    code_highlight_inject_vector()

} else {


chunk_name %>%
  chunk_code_get() %>%
  code_parse(lang = lang, omit = omit) %>%
  parsed_return_partial_code_sequence(break_type = break_type,
                                      left_assign = left_assign)

  }

}

chunk_name_return_function_sequence <- function(chunk_name,
                                            break_type = "auto",
                                            left_assign = F,
                                            lang = "r",
                                            omit = "#OMIT"){


  chunk_name %>%
    chunk_code_get() %>%
    code_parse(lang = lang, omit = omit) %>%
    parsed_return_recent_function_sequence(break_type = break_type,
                                           left_assign = left_assign)

}



#### Template code chunks to deliver partial builds on ####
return_partial_chunks_template_code <- function(){

  "```{<<<lang>>> <<<chunk_name>>>_<<<break_type>>>_<<<breaks_prep>>>_code, eval = FALSE, echo = TRUE, code = code_seq[[<<<breaks>>>]]}
```"

}

#### Template code chunks to deliver partial builds on ####
return_partial_chunks_template_code_lag <- function(){


  "```{<<<lang>>> <<<chunk_name>>>_<<<break_type>>>_<<<breaks_prep>>>_code_lag, eval = FALSE, echo = TRUE, code = code_seq_lag[[<<<breaks>>>]]}
```"

}


return_partial_chunks_template_output <- function(){

  "```{<<<lang>>> <<<chunk_name>>>_<<<break_type>>>_<<<breaks_prep>>>_output, eval = TRUE, echo = FALSE, code = code_seq[[<<<breaks>>>]]}
```"

  # , out.width = \"<<<out.width>>>\", out.height = \"<<<out.height>>>\"
}

return_partial_chunks_template_output_lag <- function(){

  "```{<<<lang>>> <<<chunk_name>>>_<<<break_type>>>_<<<breaks_prep>>>_output_lag, eval = TRUE, echo = FALSE, code = code_seq_lag[[<<<breaks>>>]]}
```"

}

return_partial_chunks_template_output_lag2 <- function(){

  "```{<<<lang>>> <<<chunk_name>>>_<<<break_type>>>_<<<breaks_prep>>>_output_lag2, eval = TRUE, echo = FALSE, code = code_seq_lag2[[<<<breaks>>>]]}
```"
}

return_partial_chunks_template_output_target <- function(){

  "```{<<<lang>>> <<<chunk_name>>>_<<<break_type>>>_<<<breaks_prep>>>_output_target, eval = TRUE, echo = FALSE, code = code_seq_target[[<<<breaks>>>]]}
```"
}

return_partial_chunks_template_output_start <- function(){

  "```{<<<lang>>> <<<chunk_name>>>_<<<break_type>>>_<<<breaks_prep>>>_output_target, eval = TRUE, echo = FALSE, code = code_seq_start[[<<<breaks>>>]]}
```"
}


return_partial_chunks_template_function <- function(){

  "```{<<<lang>>> <<<chunk_name>>>_<<<break_type>>>_<<<breaks_prep>>>_function, eval = TRUE, echo = FALSE, code = func_seq[[<<<breaks>>>]]}
```"

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
  color = c("black", "black", "black"),
  spacing = 1,
  font_size_code = 80
){

  active_in_100 <- 100 - spacing*(length(widths))
  normalized_widths <- active_in_100*widths/sum(widths)

  width_left <- NULL
  width_right <- NULL
  width_middle <- NULL

  try(width_1   <- normalized_widths[1])
  try(width_2  <- normalized_widths[2])
  try(width_3 <- normalized_widths[3])

  id <- paste0(chunk_name, "-", break_type)

  knitr::asis_output(glue::glue(
    "<style>
    .panel1-<<<id>>> {
      color: <<<color[1]>>>;
      width: <<<width_1>>>%;
      float: left;
      padding-left: 1%;
      font-size: <<<font_size_code>>>
    }
    .panel2-<<<id>>> {
      color: <<<color[2]>>>;
      width: <<<width_2>>>%;
      float: left;
      padding-left: 1%;
      font-size: <<<font_size_code>>>
    }
    .panel3-<<<id>>> {
      color: <<<color[3]>>>;
      width: <<<width_3>>>%;
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

###### Create spawned code chunks ######
chunk_expand <- function(chunk_name = "example",
                         break_type = "auto",
                         display_type = c("code", "output"),
                         num_breaks = 2,
                         title = "",
                         md = NULL,
                         md2 = NULL,
                         func = NULL,
                         lang = "r",
                         custom = F,
                         widths = c(39, 60, 0),
                         color = c("black", "black", "black"),
                         font_size_code = "80%"
){

  breaks <- 1:num_breaks
  breaks_prep <- stringr::str_pad(breaks, width = 2, pad = "0")
  code <- return_partial_chunks_template_code()
  code_lag <- return_partial_chunks_template_code_lag()
  output <- return_partial_chunks_template_output()
  output_lag <- return_partial_chunks_template_output_lag()
  output_lag2 <- return_partial_chunks_template_output_lag2()
  output_target <- return_partial_chunks_template_output_target()
  output_start <- return_partial_chunks_template_output_start()
  try(func <- return_partial_chunks_template_function()) #because not worked out for python
  md <- "`r md[<<<breaks>>>]`"
  md2 <- "`r md2[<<<breaks>>>]`"

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
      ".panel1-<<<chunk_name>>>-<<<break_type>>>[",
      left,
      "]",
      " ",
      ".panel2-<<<chunk_name>>>-<<<break_type>>>[",
      middle,
      "]",
      " ",
      ".panel3-<<<chunk_name>>>-<<<break_type>>>[",
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
      ".panel1-<<<chunk_name>>>-<<<break_type>>>[",
      left,
      "]",
      " ",
      ".panel2-<<<chunk_name>>>-<<<break_type>>>[",
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
                          color = color,
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


code_seq_create_lag <- function(code_seq, lag = 1){

  len <- length(code_seq)
  code_seq_lag <- list()

  for (i in 1:lag){
  code_seq_lag[[i]] <- "'--'" #position 1
  }

  # position 2 to length
  for (i in 1:(len - lag)){

    code_seq_lag[[i + lag]]  <- code_seq[[i]]

  }

  code_seq_lag

}


code_seq_create_target <- function(code_seq){

  len <- length(code_seq)
  code_seq_target <- list()

  for (i in 1:len){
    code_seq_target[[i]] <- code_seq[[len]]
  }

  code_seq_target

}


code_seq_create_start <- function(code_seq){

  len <- length(code_seq)
  code_seq_start <- list()

  for (i in 1:len){
    code_seq_start[[i]] <- code_seq[[1]]
  }

  code_seq_start

}

# create_ggplot_code() %>%
#   code_parse() %>%
#   parsed_return_partial_code_sequence() %>%
#   code_seq_create_target()


correct_py <- function(lang){

  if (lang == "py") {lang <- "python"}

}


######## The exported functions ############

#' Title CREA
#'
#' @param chunk_name a character string referring to the name of the source chunk for the flipbooking
#' @param break_type "auto" is default finding appropriate breakpoints, "user" can be used with the special comment message #BREAK within the source code chunk, "non_seq" can be used for non sequential display of code with special comment messages #BREAK2 (will show in second frame) and #BREAK3 (will show in third frame), an integer input can be given too, to simply display the source code chunk multiple times which is appropriate for observing multiple realizations of sampling, "rotate" allows cycling through different lines of code, the comment #ROTATE is used for lines to by cycled through
#' @param left_assign a logical, default is FALSE, if TRUE will print the object created in the upper left hand corner of the source code chunk at the end of each partial reveal
#' @param code_seq a list of code as character strings, the list will automatically be created based on the previous three arguments or the user can input code manually
#' @param num_breaks an integer, automatically calculated based on the length of the the code_seq list
#' @param display_type a character string vector, the default is c("code", "output") for code and output to be displayed side-by-side, "output" will create spawned code chunks to only display output, "code" will create spawned code chunks only to show the partial code builds; "func" and "md" may also be displayed
#' @param lang a character string indicating what programming language will be used. "r" is default; "python" is experimental
#' @param func_seq a character string with function names; default is NULL and will reflect whatever function is highlighted from the code sequence
#' @param title a character string that may contain a title for the frames of the flipbook; this may included header info "## My Title" for example is a second level markdown title in Xaringan
#' @param md a character string vector that contains markdown; each element will be shown on a separate slide in the display panel "md" (see display_type)
#' @param md2 a character string vector that contains markdown; each element will be shown on a separate slide in the display panel "md" (see display_type)
#' @param widths a numeric vector containing relative widths for panels
#' @param font_size_code this is not reliable yet, place holder!
#'
#' @return a string object is returned will only work in 'knitr' context
#' @export
#'
chunk_reveal <- function(chunk_name = NULL,
                   break_type = "auto",
                   left_assign = F,
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
                   widths = NULL,
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

    code_seq <- chunk_name_return_code_sequence(chunk_name, break_type, left_assign, lang, omit = omit)

  }

  if (is.null(func_seq) & !is.null(code_seq)){

    try(func_seq <- chunk_name_return_function_sequence(chunk_name, break_type, left_assign, lang, omit = omit))
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
                       color = color,
                       font_size_code = font_size_code
                       #,
                       #out.height = out.height,
                       #out.width = out.width
                       )

paste(knitr::knit(text = text), collapse = "\n")

}


## returning code sequence as a vector

code_seq_as_vector <- function(code_seq){

  code_seq %>%
    tibble::tibble(code = .) %>%
    tidyr::unnest() %>%
    dplyr::pull("code")

}


chunk_code_seq_as_vector <- function(chunk_name,
                                     break_type = "auto",
                                     left_assign = F,
                                     lang = "r",
                                     omit = "#OMIT"
                                     ){


  chunk_name %>%
    chunk_code_get() %>%
    code_parse(lang = lang, omit = omit) %>%
    parsed_return_partial_code_sequence(break_type = break_type,
                                        left_assign = left_assign) %>%
    code_seq_as_vector()


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
