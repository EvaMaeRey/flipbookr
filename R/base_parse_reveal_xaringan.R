# Emi Tanaka (@statsgen) and Garrick Aden-Buie (@grrrck) and Evangeline Reynolds (@EvaMaeRey)
# have contributed to this code
create_code <- function(){ # for testing w/o knitting

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
create_short_code <- function(){ # for testing w/o knitting

  "cars %>%             # the data
  filter(speed > 4) %>%  # subset #REVEAL
  ggplot() #REVEAL"

}


create_ggplot_code <- function(){ # for testing w/o knitting

  "ggplot2::ggplot(cars) +  # initiate ggplot
  ggplot2::aes(x = speed) +
  ggplot2::aes(y = dist) +
  # Describing what follows
  ggplot2::geom_point(alpha = .3) "

}


code_create_left_assign <- function(){

# for testing w/o knitting
  "my_cars <- cars %>%             # the data  #REVEAL
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


#' Parse code
#'
#' @param code code as a character string
#'
#' @return parsed code
#'
#' @examples
code_parse <- function(code) {

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




parsed_calc_show <- function(parsed, reveal_type = "user"){

  if (reveal_type == "auto") {

    code_order <- cumsum(parsed$auto) + 1 - parsed$auto
    num_panes <- max(code_order)

  } else if (reveal_type == "user") {

    code_order <- cumsum(parsed$user) + 1 - parsed$user
    num_panes <- max(code_order)

  } else if (reveal_type == "non_seq") {

    # make flexible by allowing non integers here.
    code_order <- parsed$non_seq
    num_panes <- max(code_order)

  } else if (is.numeric(reveal_type)) {  # multiverse case


    code_order <- rep(1, nrow(parsed))
    num_panes <- reveal_type

  }

  which_show <- list()

  for (i in 1:num_panes) {

    # fix this for non_sequential to allow removal
    which_show[[i]] <- which(code_order <= i)

  }

  which_show

}


# shown_lines_calc_highlight()
shown_lines_calc_highlight <- function(which_show = list(c(1, 2), c(1, 2, 3, 4)),
                                    reveal_type = "auto"){

  which_highlight <- list()

  if (reveal_type == "user" | reveal_type == "auto") {

  which_highlight[[1]] <- which_show[[1]]

    for (i in 2:length(which_show)) {

      which_highlight[[i]] <- which_show[[i]][!(which_show[[i]] %in% which_show[[i - 1]])]

    }

  }  else if (is.numeric(reveal_type)) {

      for (i in 1:length(which_show)) {

        which_highlight[[i]] <- as.integer(c())

      }

  } else if (reveal_type == "non_seq") {


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
#' @param reveal_type
#' @param which_show
#' @param which_highlight
#' @param left_assign
#'
#' @return
#' @export
#'
#' @examples
parsed_return_partial_code_sequence <- function(parsed,
                         reveal_type = "auto",
                         which_show = parsed_calc_show(parsed = parsed,
                                                       reveal_type = reveal_type),
                         which_highlight =
                           shown_lines_calc_highlight(which_show = which_show,
                                                      reveal_type = reveal_type),
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
                                            reveal_type = "auto",
                                            left_assign = F){

chunk_name %>%
  chunk_code_get() %>%
  code_parse() %>%
  parsed_return_partial_code_sequence(reveal_type = reveal_type,
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

  "```{r {{{chunk_name}}}_{{{reveal_type}}}_{{{breaks}}}_code, eval = FALSE, echo = TRUE, code = code_seq[[{{{breaks}}}]]}
  ```"

}



# Thinking about delivery to sweave/beamer
# return_partial_chunks_template_code <-
#   function(type = "code", delivery = "rmd") {
#
#     chunk_name_and_options <-
#       glue::glue(
#         "{{{chunk_name}}}_{{{reveal_type}}}_{{{breaks}}}_{{{{type}}}},",
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

"```{r {{{chunk_name}}}_{{{reveal_type}}}_{{{breaks}}}_output, eval = TRUE, echo = FALSE, code = code_seq[[{{{breaks}}}]]}
```"

}



#' Title
#'
#' @param chunk_name
#' @param reveal_type
#' @param left_assign
#' @param display_type
#' @param split
#'
#' @return
#' @export
#'
#' @examples
#' chunk_expand()
#' chunk_expand(reveal_type = "user", display_type = "code")
#' chunk_expand(reveal_type = "non_seq", display_type = "output")
chunk_expand <- function(chunk_name = "example",
                                  reveal_type = "auto",
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
#' @param reveal_type
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
reveal <- function(chunk_name = "example_name",
                   reveal_type = "auto",
                   left_assign = F,
                   code_seq = chunk_name_return_code_sequence(chunk_name, reveal_type, left_assign),
                   num_breaks = length(code_seq),
                   display_type = "both",
                   split = 40){

  text <- chunk_expand(chunk_name = chunk_name,
                       reveal_type = reveal_type,
                       num_breaks = num_breaks,
                       display_type = display_type,
                       split = split)

  paste(knitr::knit(text = text), collapse = "\n")


}





#'
#' # flipbook mini - build a gif flipbook using cowplot.
#'
#' #' Title
#' #'
#' #' @param code
#' #' @param upto
#' #' @param highlight
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' # create_ggplot_code() %>%
#' # code_parse() %>%
#' # parsed_return_partial_code_sequence() %>%
#' # .[[2]] %>%
#' # build_partial_code_plot()
#' build_partial_code_plot_mini <- function(code_w_highlight, upto = 8, highlight = 1:8){
#'
#'   writeLines(text = code_w_highlight,
#'              con  = "tmp.R")
#'   source("tmp.R")
#'
#' }
#'
#'
#'
#' #' Title
#' #'
#' #' @param code
#' #' @param upto
#' #' @param highlight
#' #' @param highlight_color
#' #' @param font_size
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' # create_ggplot_code() %>%
#' # code_parse() %>%
#' # parsed_return_partial_code_sequence() %>%
#' # .[[4]] %>%
#' # build_partial_code_text_plot_mini(num_lines = 8, font_size = 6)
#' build_partial_code_text_plot_mini <- function(code_w_highlight,
#'                                          highlight_color = "plum4",
#'                                          font_size = 4,
#'                                          num_lines = 16) {
#'
#'   code_w_highlight %>%
#'     dplyr::tibble() %>%
#'     dplyr::mutate(highlight = stringr::str_detect(., "#<<")) %>%
#'     dplyr::mutate(code_as_text = stringr::str_remove(., "#<<")) %>%
#'     dplyr::mutate(n = 1:dplyr::n()) ->
#'   prepped
#'
#'   width <- 74
#'   height <- 1
#'
#'   ggplot2::ggplot(data = prepped) +
#'     ggplot2::aes(x = 40) +
#'     ggplot2::aes(y = n) +
#'     ggplot2::scale_y_reverse(limits = c(num_lines, 0)) +
#'     ggplot2::scale_x_continuous(limits = c(0, 80), expand = c(0, 0)) +
#'     ggplot2::coord_fixed(ratio = 3.5) +
#'     ggplot2::aes(label = code_as_text) +
#'     # grey background
#'     ggplot2::geom_rect(ggplot2::aes(ymin = n - .8, ymax = n + .8),
#'                        fill = "grey95",
#'                        xmin = 0, xmax = 78) +
#'     ggplot2::geom_rect(ggplot2::aes(ymin = n - .6, ymax = n + .6),
#'                        fill = "grey90",
#'                        xmin = 1, xmax = 77) +
#'     # highlighting
#'     ggplot2::geom_rect(data = prepped %>% dplyr::filter(highlight),
#'                        ggplot2::aes(ymin = n - .5, ymax = n + .5),
#'                        xmin = 1.5, xmax = 76.5,
#'                        fill = highlight_color,
#'                        width = width - 2,
#'                        height = 1,
#'                        alpha = .5) +
#'     ggplot2::labs(fill = NULL) +
#'     ggplot2::geom_text(x = 3, hjust = 0, family = "mono",
#'                        size = font_size, color = "grey22") +
#'     ggplot2::theme_void() +
#'     ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0, "cm"))
#'
#' }
#'
#' #' Title
#' #'
#' #' @param code_w_highlight
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' #' create_ggplot_code() %>%
#' #' code_parse() %>%
#' #' parsed_return_partial_code_sequence() %>%
#' #' .[[2]] %>%
#' #' create_cow_frame()
#' create_cow_frame <- function(code_w_highlight, title = "flipbook mini") {
#'
#'   the_plot <- build_partial_code_plot_mini(code_w_highlight)[[1]]
#'
#'   text_plot <- build_partial_code_text_plot_mini(code_w_highlight)
#'
#'   a_title <- cowplot::ggdraw() +
#'     cowplot::draw_label(label = title, fontface = 'bold')
#'
#'   # the case of code and plots
#'   if (show_code == T) {
#'
#'     side_by_side <- cowplot::plot_grid(text_plot,
#'                                        the_plot,
#'                                        rel_widths = c(1, 1))
#'     cowplot::plot_grid(a_title,
#'                        side_by_side,
#'                        rel_heights = c(0.1, 1),
#'                        ncol = 1)
#'
#'
#'   }
#'
#' }
#'
#'
#'
#' pngs_to_gif <- function(path, file_out){
#'
#'   files <- list.files(path = path, pattern = paste(".png"))
#'   files_path <- paste0(path, "/", files)
#'
#'   files_path %>%
#'     file.info() %>%
#'     rownames_to_column(var = "file") %>%
#'     arrange(mtime) %>% # sort them by time modified
#'     pull(file) %>%
#'     purrr::map(magick::image_read) %>% # reads each path file
#'     magick::image_join() %>% # joins image
#'     magick::image_animate(fps = 1) %>% # animates
#'     magick::image_write(path = file_out)
#'
#' }
#'
#'
