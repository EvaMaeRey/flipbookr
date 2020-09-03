
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

  } else if (break_type == "replacement") {

    num_panes <- length(replacements)

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

  } else if (break_type == "replacement")  {
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

  if (break_type == "replacement") {

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
    } else {
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
                                            omit = "#OMIT",
                                            replacements = NULL,
                                            replace = NULL,
                                            replacements2 = NULL,
                                            replace2 = NULL,
                                            replacements3 = NULL,
                                            replace3 = NULL){

  if (break_type == "replacement"){

    chunk_name %>%
      chunk_code_get() %>%
      code_replacements_and_highlight(replace = replace,
                                      replacements = replacements,
                                      replace2 = replace2, replacements2 = replacements2,
                                      replace3 = replace3, replacements3 = replacements3)

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

#
#
# create_code() %>%
#   code_replacements_and_highlight(replacements = 1:8/8, replace = "\\.3")

code_replacements_and_highlight <- function(code,
                                            replacements = 1:3, replace = NULL,
                                            replacements2 = 4:6, replace2 = NULL,
                                            replacements3 = 4:6, replace3 = NULL){

  replacements <- as.character(replacements)
  replacements2 <- as.character(replacements2)
  replacements3 <- as.character(replacements3)

  code_seq <- list()

  for (i in 1:length(replacements)){

    code_seq[[i]] <- code %>%
      code_as_table() %>%
      dplyr::mutate(code = ifelse(stringr::str_detect(raw_code, replace),
                                  paste(raw_code, "#<<"),
                                  raw_code)) %>%
      dplyr::mutate(code =
                      stringr::str_replace_all(code,
                                               replace,
                                               replacements[i])) %>%
      dplyr::mutate(code = ifelse(stringr::str_detect(raw_code, replace2),
                                  paste(raw_code, "#<<"),
                                  raw_code)) %>%
      dplyr::mutate(code =
                      stringr::str_replace_all(code,
                                               replace2,
                                               replacements2[i])) %>%
      dplyr::mutate(code = ifelse(stringr::str_detect(raw_code, replace3),
                                  paste(raw_code, "#<<"),
                                  raw_code)) %>%
      dplyr::mutate(code =
                      stringr::str_replace_all(code,
                                               replace3,
                                               replacements3[i])) %>%
      dplyr::pull(code)

  }

  code_seq

}


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



# create_ggplot_code() %>%
#   code_parse() %>%
#   parsed_return_partial_code_sequence() %>%
#   code_seq_create_target()


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


## returning code sequence as a vector

code_seq_as_vector <- function(code_seq){

  code_seq %>%
    tibble::tibble(code = .) %>%
    tidyr::unnest() %>%
    dplyr::pull("code")

}


text_segment <- function(text, sep){

  text %>%
    stringr::str_split(pattern = sep) %>%
    .[[1]]

}


