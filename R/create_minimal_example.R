
#' Create Rmd example
#'
#' @param path a character string to save out the minimal example
#'
#' @return a .Rmd
#' @export
#'
#' @examples
create_minimal_example <- function(path, open = FALSE){
  usethis::use_template(template = "minimal_example", save_as = path, open = open)
}
