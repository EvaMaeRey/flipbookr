
####### Get code from source chunk #####

chunk_code_get <- function(chunk_name){

  paste(knitr::knit_code$get(chunk_name), collapse = "\n")

}


correct_py <- function(lang){

  if (lang == "py") {lang <- "python"}

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
