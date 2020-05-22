define_css <- function(
  chunk_name = "example",
  break_type = "auto",
  custom = FALSE,
  width_code = "38%",
  width_plot = "60%"
){
  id <- paste0(chunk_name, "-", break_type)

  knitr::asis_output(glue::glue(
    "<style>
    .left-code-<<<id>>> {
      color: #777;
      width: <<<width_code>>>;
      height: 92%;
      float: left;
    }
    .right-output-<<<id>>> {
      width: <<<width_plot>>>;
      float: right;
      padding-left: 1%;
    }
    </style>
    ",
    .open = "<<<",
    .close = ">>>",
    .sep = "\n"
  ))
}

# return_css()

# create_code() %>%
#   code_parse() %>%
#   parsed_return_partial_code_sequence()

# "code_chunk_name" %>%
#   chunk_expand(lang = "python")
###### Create spauned code chunks ######
chunk_expand <- function(chunk_name = "example",
                         break_type = "auto",
                         display_type = "both",
                         num_breaks = 2,
                         split = 40,
                         title = "",
                         lang = "r",
                         custom = F,
                         width_code = "38%",
                         width_plot = "60%"#,
                         #out.width = "70%",
                         #out.height = "70%"
){

  breaks <- 1:num_breaks

  if (display_type == "both") {

    partial_knit_steps <- glue::glue(
      "class: split-<<<split>>>",
      "count: false",
      " ",
      title,
      ".left-code-<<<chunk_name>>>-<<<break_type>>>[",
      return_partial_chunks_template_code(),
      "]",
      " ",
      ".right-output-<<<chunk_name>>>-<<<break_type>>>[",
      return_partial_chunks_template_output(),
      "]",
      " ",
      .open = "<<<", .close = ">>>", .sep = "\n"
    )

  } else if (display_type == "code" | display_type == "output") {

    if (display_type == "output") {

      chunk <- return_partial_chunks_template_output()

    } else {

      chunk <- return_partial_chunks_template_code()

    }

    partial_knit_steps <- glue::glue(
      "count: false",
      title,
      chunk,
      .open = "<<<", .close = ">>>", .sep = "\n"
    )

  }

  defined_css <- define_css(chunk_name = chunk_name,
                            break_type = break_type,
                            width_code = width_code,
                            width_plot = width_plot)

  slide_code <- glue::glue_collapse(partial_knit_steps, sep = "\n\n---\n")

  glue::glue("{slide_code}\n\n{defined_css}\n\n", .trim = FALSE)


}
