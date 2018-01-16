dfiddle <- function(){
  if(!is_rstudio_selection()) return(invisible(NULL))

  doc_context <- rstudioapi::primary_selection(rstudioapi::getActiveDocumentContext())
  eval_env <- new.env()

  if(is_naked_vec(doc_context)){
    regular_delimited <-
      paste0(split_naked_vector(doc_context)[[1]], collapse=", ")
    vector_form <- paste0("c(",regular_delimited,")")
  } # parse to a vector

  is_horiz_vector # pivot to vertical

  is_vert_vector # pivot to quoted horiz vector

  is_quoted_horiz_vector # pivot to quoted vertical vector

  is_quoted_vert_vector # pivot to unqoted horizontal

  selection_result <-
    tryCatch( expr = eval(parse(text = doc_context$text), envir = eval_env),
              error = NULL)

  if(tibble::is_tibble(selection_result) |
     is.data.frame(selection_result)){ df_paste(selection_result) }


}

is_naked_vec <- function(text){
 text <- trimws(text)
 naked_vector <- "^(((\"|\')[^\'\"]+(\"\')|\\w+)(\\s)*(,)*(\n)*(\\s)*)+"
 grep(x = text, pattern = naked_vector) == 1
}

split_naked_vector<- function(naked_vec){
  strsplit(x = naked_vec, split = "(?<=(\"|'|\\w))[\\s,\n]+",
           perl = TRUE)
}
