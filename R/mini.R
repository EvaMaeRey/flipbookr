############################## mini #############################


#### create plot from partial code ####
partial_plotting_code_plot <- function(partial_code_w_highlight){

  writeLines(text = partial_code_w_highlight,
             con  = "tmp.R")
  source("tmp.R")

}


#### plotting text of code as a ggplot ####
partial_plotting_code_prep_for_plot_text <- function(partial_code_w_highlight){

  partial_code_w_highlight %>%
    dplyr::tibble() %>%
    dplyr::mutate(highlight = stringr::str_detect(., "#<<")) %>%
    dplyr::mutate(code_as_text = stringr::str_remove(., "#<<")) %>%
    dplyr::mutate(n = 1:dplyr::n())


}


prepped_partial_plotting_code_plot_text <- function(prepped_plotting_code,
                                                    highlight_color = "plum4",
                                                    font_size = 7,
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


#### Build single frame ####
create_cow_frame <- function(partial_code_w_highlight,
                             title = "flipbook mini",
                             highlight_color = "plum4",
                             font_size = 7,
                             num_lines = 16,
                             display_type = "both") {

  the_plot <- partial_plotting_code_plot(partial_code_w_highlight)[[1]]

  text_plot <- partial_code_w_highlight %>%
    partial_plotting_code_prep_for_plot_text() %>%
    prepped_partial_plotting_code_plot_text(highlight_color,
                                            font_size,
                                            num_lines)

  a_title <- cowplot::ggdraw() +
    cowplot::draw_label(label = title, fontface = 'bold')

  # the case of code and plots
  if (display_type == "both") {

  side_by_side <- cowplot::plot_grid(text_plot,
                                     the_plot,
                                     rel_widths = c(1, 1))
  cowplot::plot_grid(a_title,
                     side_by_side,
                     rel_heights = c(0.1, 1),
                     ncol = 1)

  } else if (display_type == "code") {

    text_plot
  } else if (display_type == "output") {

    the_plot
   }

}


#### Create all the frames ####

code_seq_build_and_save_all_cow_frames <- function(code_seq,
                                                   id = "",
                                                   dir = paste0("temp_mini_figures", id),
                                                   title = "flipbook mini",
                                                   highlight_color = "plum4",
                                                   font_size = 7,
                                                   num_lines = 16,
                                                   display_type = "both"){


  dir.create(path = dir)

  for (i in 1:length(code_seq)) {

    code_seq %>%
      .[[i]] %>%
      create_cow_frame(title,
                       highlight_color,
                       font_size,
                       num_lines,
                       display_type) ->
      the_composite_plot

    cowplot::save_plot(filename = paste0(dir, "/frame", i, ".png"), plot = the_composite_plot)

  }



}


#### combine frames into a gif ####
# "temp_mini_figures" %>%
#   pngs_to_gif()
pngs_to_gif <- function(dir = "temp_mini_figures",
                        file_out = "temp_mini.gif",
                        delete_figs_dir = F,
                        fps = 1){

  files <- list.files(path = dir, pattern = paste(".png"))
  files_path <- paste0(dir, "/", files)

  # png("frame%03d.png")
  # par(ask = FALSE)
  # for(i in 1:10)
  #   plot(rnorm(i * 10), main = i)
  # dev.off()
  # png_files <- sprintf("frame%03d.png", 1:10)
  # gif_file <- gifski::gifski(files_path)
  # unlink(dir)
  # utils::browseURL(gif_file)
  #

  files_path %>%
    file.info() %>%
    tibble::rownames_to_column(var = "file") %>%
    dplyr::arrange(mtime) %>% # sort them by time modified
    dplyr::pull(file) %>%
    purrr::map(magick::image_read) %>% # reads each path file
    magick::image_join() %>% # joins image
    magick::image_animate(fps = fps) %>% # animates
    magick::image_write(path = file_out)

  # if (delete_figs_dir) {}

}


#### code to flipbook mini gif ####
# create_ggplot_code() %>%
#  code_create_gif_flipbook()
code_create_gif_flipbook <- function(code,
                                     id = "",
                                     dir = paste0("temp_mini_figures", id),
                                     file_out = paste0("temp_mini",id,display_type,".gif"),
                                     title = "flipbook mini created with {flipbookr}",
                                     highlight_color = "plum4",
                                     font_size = 3,
                                     num_lines = 16,
                                     display_type = "both",
                                     break_type = "auto",
                                     which_show = parsed_calc_show(parsed = code_parse(code),
                                                                   break_type = break_type),
                                     which_highlight =
                                       shown_lines_calc_highlight(which_show = which_show,
                                                                  break_type = break_type),
                                     left_assign = F,
                                     fps = 1){

  code %>%
    code_parse() %>%
    parsed_return_partial_code_sequence(break_type,
                                        which_show,
                                        which_highlight,
                                        left_assign) %>%
    code_seq_build_and_save_all_cow_frames(id,
                                           dir,
                                           title,
                                           highlight_color,
                                           font_size = font_size,
                                           num_lines,
                                           display_type)

  pngs_to_gif(dir = dir,
              file_out = file_out,
              delete_figs_dir = F,
              fps)

}



chunk_create_gif_flipbook <- function(...){

  chunk_code_get() %>%
    code_create_gif_flipbook(...)

}


chunk_gif_flipbook_embed <- function(chunk_name,
                                     id = chunk_name,
                                     display_type = "both",
                                     title = "mini"){

  chunk_name %>%
    chunk_code_get() %>%
    code_create_gif_flipbook(id = chunk_name,
                             display_type = display_type,
                             title = title) # creates temp_mini.gif

  knitr::include_graphics(paste0("temp_mini", chunk_name, display_type,".gif"))

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



#
# You can try a flipbook mini too...
#
# ```{r my_source_code_chunk, eval = F}
# ggplot(data = cars) +
#   aes(x = speed) +
#   aes(y = dist) +
#   geom_point(alpha = .3,
#              color = "blue")
# ```
#
# ---
#
#   ```{r pipeline_to_gif, include = F}
# # "my_source_code_chunk" %>% # name of source code chunk
# #   chunk_create_gif_flipbook() # embed flipbook gif in html
# ```
#
# r chunk_reveal("pipeline_to_gif")`


