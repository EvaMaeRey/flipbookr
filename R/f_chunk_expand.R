
# create_code() %>%
#   code_parse() %>%
#   parsed_return_partial_code_sequence()

# "code_chunk_name" %>%
#   chunk_expand(lang = "python")

###### Create spawned code chunks ######
chunk_expand <- function(chunk_name = "example",
                         break_type = "auto",
                         display_type = c("code", "output"),
                         chunk_options = "fig.height = 12",
                         num_breaks = 2,
                         title = "",
                         md = NULL,
                         md2 = NULL,
                         func = NULL,
                         lang = "r",
                         custom = F,
                         widths = c(39, 60, 0),
                         color = c("black", "black", "black"),
                         font_size_code = "80%",
                         float = "left"
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
                                font_size_code = font_size_code,
                                float = float
  )

  slide_code <- glue::glue_collapse(partial_knit_steps, sep = "\n\n---\n")

  glue::glue("{slide_code}\n\n{the_defined_css}\n\n", .trim = FALSE)


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
