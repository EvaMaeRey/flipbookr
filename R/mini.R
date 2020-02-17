############################## mini #############################


#' # flipbook mini - build a gif flipbook using "cowplot" as presentation delivery
#'
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
# create_ggplot_code() %>%
#   code_parse() %>%
#   parsed_return_partial_code_sequence() %>%
#   .[[2]] %>%
#   partial_plotting_code_plot()
partial_plotting_code_plot <- function(partial_code_w_highlight){

  writeLines(text = partial_code_w_highlight,
             con  = "tmp.R")
  source("tmp.R")

}


# create_ggplot_code() %>%
#   code_parse() %>%
#   parsed_return_partial_code_sequence() %>%
#   .[[2]] %>%
#   partial_plotting_code_prep_for_plot_text()

partial_plotting_code_prep_for_plot_text <- function(partial_code_w_highlight){

  partial_code_w_highlight %>%
    dplyr::tibble() %>%
    dplyr::mutate(highlight = stringr::str_detect(., "#<<")) %>%
    dplyr::mutate(code_as_text = stringr::str_remove(., "#<<")) %>%
    dplyr::mutate(n = 1:dplyr::n())


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
# create_ggplot_code() %>%
#   code_parse() %>%
#   parsed_return_partial_code_sequence() %>%
#   .[[4]] %>%
#   partial_plotting_code_prep_for_plot_text() %>%
#   prepped_partial_plotting_code_plot_text()
prepped_partial_plotting_code_plot_text <- function(prepped_plotting_code,
                                                    highlight_color = "plum4",
                                                    font_size = 5,
                                                    num_lines = 16) {


  width <- 400
  height <- 1

  prepped_plotting_code %>%
    ggplot2::ggplot() +
    ggplot2::aes(x = 0) +
    ggplot2::aes(y = n) +
    ggplot2::scale_y_reverse(limits = c(num_lines, 0)) +
    ggplot2::scale_x_continuous(limits = c(0, 400)) +
    ggplot2::coord_fixed(ratio = 30) +
    ggplot2::aes(label = code_as_text) +
    # grey background
    ggplot2::geom_rect(ggplot2::aes(ymin = n - .8, ymax = n + .8),
                       fill = "grey95",
                       xmin = -10, xmax = width) +
    ggplot2::geom_rect(ggplot2::aes(ymin = n - .6, ymax = n + .6),
                       fill = "grey90",
                       xmin = -5, xmax = width - 5) +
    # highlighting
    ggplot2::geom_rect(data = prepped_plotting_code %>% dplyr::filter(highlight),
                       ggplot2::aes(ymin = n - .5, ymax = n + .5),
                       xmin = -2 , xmax = width - 12,
                       fill = highlight_color,
                       alpha = .5) +
    ggplot2::labs(fill = NULL) +
    ggplot2::geom_text(x = 3, hjust = 0, family = "mono",
                       size = font_size, color = "grey22") +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0, "cm"))

}



#' Title
#'
#' @param partial_code_w_highlight
#'
#' @return
#' @export
#'
#' @examples
# create_ggplot_code() %>%
#   code_parse() %>%
#   parsed_return_partial_code_sequence() %>%
#   .[[2]] %>%
#   create_cow_frame()
create_cow_frame <- function(partial_code_w_highlight,
                             title = "flipbook mini",
                             highlight_color = "plum4",
                             font_size = 5,
                             num_lines = 16) {

  the_plot <- partial_plotting_code_plot(partial_code_w_highlight)[[1]]

  text_plot <- partial_code_w_highlight %>%
    partial_plotting_code_prep_for_plot_text() %>%
    prepped_partial_plotting_code_plot_text(highlight_color,
                                            font_size,
                                            num_lines)

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

}



# create_ggplot_code() %>%
#   code_parse() %>%
#   parsed_return_partial_code_sequence() %>%
#   code_seq_build_and_save_all_cow_frames()
code_seq_build_and_save_all_cow_frames <- function(code_seq,
                                                   dir = "temp_mini_figures",
                                                   title = "flipbook mini",
                                                   highlight_color = "plum4",
                                                   font_size = 5,
                                                   num_lines = 16){


  dir.create(path = dir)

  for (i in 1:length(code_seq)) {

    code_seq %>%
      .[[i]] %>%
      create_cow_frame(title,
                       highlight_color,
                       font_size,
                       num_lines) ->
      the_plot

    cowplot::save_plot(filename = paste0(dir, "/frame", i, ".png"), plot = the_plot)

  }



}



# pngs_to_gif()
pngs_to_gif <- function(dir = "temp_mini_figures",
                        file_out = "temp_mini.gif",
                        delete_figs_dir = F){

  files <- list.files(path = dir, pattern = paste(".png"))
  files_path <- paste0(dir, "/", files)

  files_path %>%
    file.info() %>%
    tibble::rownames_to_column(var = "file") %>%
    dplyr::arrange(mtime) %>% # sort them by time modified
    dplyr::pull(file) %>%
    purrr::map(magick::image_read) %>% # reads each path file
    magick::image_join() %>% # joins image
    magick::image_animate(fps = 1) %>% # animates
    magick::image_write(path = file_out)

  if (delete_figs_dir) {}

}



# create_ggplot_code() %>%
# code_create_gif_flipbook()
code_create_gif_flipbook <- function(code,
                                     dir = "temp_mini_figures",
                                     file_out = "temp_mini.gif",
                                     title = "flipbook mini created with {flipbookr}",
                                     highlight_color = "plum4",
                                     font_size = 2,
                                     num_lines = 16,
                                     break_type = "auto",
                                     which_show = parsed_calc_show(parsed = code_parse(code),
                                                                   break_type = break_type),
                                     which_highlight =
                                       shown_lines_calc_highlight(which_show = which_show,
                                                                  break_type = break_type),
                                     left_assign = F
){

  code %>%
    code_parse() %>%
    parsed_return_partial_code_sequence(break_type,
                                        which_show,
                                        which_highlight,
                                        left_assign) %>%
    code_seq_build_and_save_all_cow_frames(dir,
                                           title,
                                           highlight_color,
                                           font_size = font_size,
                                           num_lines)

  pngs_to_gif()

}


#
# # Flipbook Mini - only plots supported by cowplot...
#
# We're using a delivery system of the composed cowplot. Experimental!
#
# ```{r mini, include = F}
# ggplot(cars) +
# aes(x = speed) +
# aes(y = dist) +
# geom_point() +
# aes(color = speed) +
# scale_color_viridis_c() +
# theme_minimal() +
# theme(legend.position = c(.07, .7)) +
# theme(legend.background = element_rect(fill = "white")) +
# theme(legend.background = element_rect(color = "grey96")) +
# theme(panel.grid.minor = element_blank()) +
# theme(panel.grid.major = element_line(color = "grey96")) +
# theme(text = element_text(color = "grey20"))
# ```
#
# ```{r}
# chunk_code_get("mini") %>%
# code_create_gif_flipbook()
# knitr::include_graphics("temp_mini.gif")
# ```
#
