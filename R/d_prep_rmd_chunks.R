

#### Template code chunks to deliver partial builds on ####
return_partial_chunks_template_code <- function(){

  "```{<<<lang>>> <<<chunk_name>>>_<<<break_type>>>_<<<breaks_prep>>>_code, eval = FALSE, echo = TRUE, code = code_seq[[<<<breaks>>>]]}
```"

}

return_partial_chunks_template_code_lag <- function(){


  "```{<<<lang>>> <<<chunk_name>>>_<<<break_type>>>_<<<breaks_prep>>>_code_lag, eval = FALSE, echo = TRUE, code = code_seq_lag[[<<<breaks>>>]]}
```"

}


return_partial_chunks_template_output <- function(){

  "```{<<<lang>>> <<<chunk_name>>>_<<<break_type>>>_<<<breaks_prep>>>_output, eval = TRUE, echo = FALSE, code = code_seq[[<<<breaks>>>]]}
```"

  # , out.width = \"<<<out.width>>>\", out.height = \"<<<out.height>>>\"
}

return_partial_chunks_template_output_lag <- function(){

  "```{<<<lang>>> <<<chunk_name>>>_<<<break_type>>>_<<<breaks_prep>>>_output_lag, eval = TRUE, echo = FALSE, code = code_seq_lag[[<<<breaks>>>]]}
```"

}

return_partial_chunks_template_output_lag2 <- function(){

  "```{<<<lang>>> <<<chunk_name>>>_<<<break_type>>>_<<<breaks_prep>>>_output_lag2, eval = TRUE, echo = FALSE, code = code_seq_lag2[[<<<breaks>>>]]}
```"
}

return_partial_chunks_template_output_target <- function(){

  "```{<<<lang>>> <<<chunk_name>>>_<<<break_type>>>_<<<breaks_prep>>>_output_target, eval = TRUE, echo = FALSE, code = code_seq_target[[<<<breaks>>>]]}
```"
}

return_partial_chunks_template_output_start <- function(){

  "```{<<<lang>>> <<<chunk_name>>>_<<<break_type>>>_<<<breaks_prep>>>_output_target, eval = TRUE, echo = FALSE, code = code_seq_start[[<<<breaks>>>]]}
```"
}


return_partial_chunks_template_function <- function(){

  "```{<<<lang>>> <<<chunk_name>>>_<<<break_type>>>_<<<breaks_prep>>>_function, eval = TRUE, echo = FALSE, code = func_seq[[<<<breaks>>>]]}
```"

}
