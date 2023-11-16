# Awesome!
check_is_live <- function(){

  is_live <- FALSE

  # Check to see if we're in editor context
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {

    is_live <- tryCatch({
      rstudioapi::getSourceEditorContext()
      TRUE
    }, error = function(e) FALSE)

  }

  return(is_live)

}

# so cool!
text_chunk_extract <- function(.text, chunk_name) {

  # Find the start of the desired chunk
  chunk_regex <- paste0('\\`\\`\\`\\{[A-z]+ ', chunk_name, '(\\}|(,.*\\}))$')

  start_chunk <- .text %>%
    str_which(chunk_regex)

  if (length(start_chunk) == 0) {

    stop(paste0("Error: No chunk found with name '", chunk_name, "'"))

  } else if (length(start_chunk) > 1) {

    stop(paste0("Error: Duplicate chunk name '", chunk_name, "'"))

  }

  end_chunk <- .text[-c(1:start_chunk)] %>%
    str_which(fixed("```")) %>%
    min() + start_chunk

  chunk_text <- .text[(start_chunk):(end_chunk)] %>%
    str_c(collapse = "\n")

  attributes(chunk_text) <- NULL

  return(chunk_text)

}

chunk_remove_fencing_and_options <- function(code_chunk){

  # does not yet, in fact, remove options like these:
  # | my-chunk, echo = FALSE, fig.width = 10,
  # | fig.cap = "This is a long long
  # |   long long caption."

  chunk_as_vec <- str_split(code_chunk,"\\n")[[1]]

  # remove fencing which are first and last lines
  return(chunk_as_vec[2:(length(chunk_as_vec)-1)])

}

# wow!
return_chunk_code_live <- function(chunk_name) {


  ed        <- rstudioapi::getSourceEditorContext()
  source    <- ed$contents

  # can we use knitr tools to directly parse source for us?
  # tmp       <- tempfile()
  # writeLines(source, tmp)
  # readLines(tmp)
  # knitr::knit_code$get(name = tmp)

  my_code_chunk  <- text_chunk_extract(.text = source, chunk_name)

  # If neither of those worked, error
  if (is.null(my_code_chunk)) {

    stop(paste0("Error: No chunk found with name '", chunk_name, "'"))

  }

  # remove chunk fencing, first and last lines
  my_code <- chunk_remove_fencing_and_options(my_code_chunk)

  return(my_code)

}

#' Title
#'
#' @param chunk_name a character string with the name of the chunk of interest
#'
#' @return a vector of the code contained in the referenced chunk
#' @export
#'
#' @examples
return_chunk_code <- function(chunk_name){

  is_live <- check_is_live()

  if(is_live){
    return_chunk_code_live(chunk_name)
  }else{
    knitr::knit_code$get(name = chunk_name) %>% as.vector()
  }

}
