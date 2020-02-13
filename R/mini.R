# flipbook mini - build a gif flipbook using "cowplot" as presentation delivery

#' Title
#'
#' @param code
#' @param upto
#' @param highlight
#'
#' @return
#' @export
#'
#' @examples
create_ggplot_code() %>%
  code_parse() %>%
  parsed_return_partial_code_sequence() %>%
  .[[2]] %>%
  build_partial_code_plot()
build_partial_code_plot <- function(code_w_highlight){

  writeLines(text = code_w_highlight,
             con  = "tmp.R")
  source("tmp.R")

}
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
#' create_ggplot_code() %>%
#'   code_parse() %>%
#'   parsed_return_partial_code_sequence() %>%
#'   .[[4]] %>%
#'   build_partial_code_text_plot(num_lines = 8, font_size = 6)
build_partial_code_text_plot <- function(code_w_highlight,
                                         highlight_color = "plum4",
                                         font_size = 4,
                                         num_lines = 16) {

  code_w_highlight %>%
    dplyr::tibble() %>%
    dplyr::mutate(highlight = stringr::str_detect(., "#<<")) %>%
    dplyr::mutate(code_as_text = stringr::str_remove(., "#<<")) %>%
    dplyr::mutate(n = 1:dplyr::n()) ->
    prepped

  width <- 74
  height <- 1

  ggplot2::ggplot(data = prepped) +
    ggplot2::aes(x = 40) +
    ggplot2::aes(y = n) +
    ggplot2::scale_y_reverse(limits = c(num_lines, 0)) +
    ggplot2::scale_x_continuous(limits = c(0, 80), expand = c(0, 0)) +
    ggplot2::coord_fixed(ratio = 3.5) +
    ggplot2::aes(label = code_as_text) +
    # grey background
    ggplot2::geom_rect(ggplot2::aes(ymin = n - .8, ymax = n + .8),
                       fill = "grey95",
                       xmin = 0, xmax = 78) +
    ggplot2::geom_rect(ggplot2::aes(ymin = n - .6, ymax = n + .6),
                       fill = "grey90",
                       xmin = 1, xmax = 77) +
    # highlighting
    ggplot2::geom_rect(data = prepped %>% dplyr::filter(highlight),
                       ggplot2::aes(ymin = n - .5, ymax = n + .5),
                       xmin = 1.5, xmax = 76.5,
                       fill = highlight_color,
                       width = width - 2,
                       height = 1,
                       alpha = .5) +
    ggplot2::labs(fill = NULL) +
    ggplot2::geom_text(x = 3, hjust = 0, family = "mono",
                       size = font_size, color = "grey22") +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0, "cm"))

}



#' Title
#'
#' @param code_w_highlight
#'
#' @return
#' @export
#'
#' @examples
#' create_ggplot_code() %>%
#'   code_parse() %>%
#'   parsed_return_partial_code_sequence() %>%
#'   .[[2]] %>%
#'   create_cow_frame()
create_cow_frame <- function(code_w_highlight, title = "flipbook mini") {

  the_plot <- build_partial_code_plot(code_w_highlight)[[1]]

  text_plot <- build_partial_code_text_plot(code_w_highlight)

  a_title <- cowplot::ggdraw() +
    cowplot::draw_label(label = title, fontface = 'bold')

  # the case of code and plots
  # if (display_type == "both") {

  side_by_side <- cowplot::plot_grid(text_plot,
                                     the_plot,
                                     rel_widths = c(1, 1))
  cowplot::plot_grid(a_title,
                     side_by_side,
                     rel_heights = c(0.1, 1),
                     ncol = 1)


  # }

}


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
