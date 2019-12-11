#' #' Reveal different realizations of the same code-output frames several times
#' #'
#' #' @param chunk_name a character string which is a code chunk name
#' #' @param num_reveal an integer the number of times to repeat the revelation
#' #' @param seed a vector with length
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' reveal_sequence <- function(chunk_name,
#'                             num_reveal = 10,
#'                             seed = sample(1000:2000,
#'                                           num_reveal,
#'                                           replace = F)) {
#'   # Create slide for lines 1:N for each line N in the given chunk
#'   # break_points <- seq_along(knitr:::knit_code$get(chunk_name)) # original code, breaks for each line
#'
#'   out <- glue::glue(
#'     "class: split-40",
#'     "count: false",
#'     "```{r set_{{chunk_name}}_{{seed}}_1, echo = F}",
#'     "set.seed({{seed}})",
#'     "```",
#'     ".column[.content[",
#'     "```{r code_{{chunk_name}}_{{seed}}, eval=FALSE, code=knitr::knit_code$get('{{chunk_name}}')}",
#'     "```",
#'     "]]",
#'     # "```{r set_{{chunk_name}}_{{seed}}_2, echo = F}",
#'     # "set.seed({{seed}})",
#'     # "```",
#'     ".column[.content[",
#'     "```{r output_{{chunk_name}}_{{seed}}, echo=FALSE, code=knitr::knit_code$get('{{chunk_name}}'), , comment='   '}",
#'     "```",
#'     "]]",
#'     .open = "{{", .close = "}}", .sep = "\n"
#'   )
#'
#'   glue::glue_collapse(x = out, sep = "\n---\n")
#' }
#'
#' #' Title
#' #'
#' #' @param chunk_name a character string which is a code chunk name
#' #' @param num_reveal an integer the number of times to repeat the revelation
#' #' @param seed a vector with length
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' apply_repeat_reveal <- function(chunk_name,
#'                                 num_reveal = 10,
#'                                 seed = sample(1000:2000,
#'                                               num_reveal,
#'                                               replace = F)){
#'   paste(knitr::knit(text = reveal_sequence(chunk_name,
#'                                            num_reveal,
#'                                            seed)), collapse = "\n")
#' }
