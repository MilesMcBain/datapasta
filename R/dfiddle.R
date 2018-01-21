dfiddle <- function(){
  if(!is_rstudio_selection()) return(invisible(NULL))

  doc_context <- rstudioapi::primary_selection(rstudioapi::getActiveDocumentContext())
  eval_env <- new.env()

  if(is_naked_vec(doc_context$text)){
    indent_context <- attr(regexpr(pattern = "^\\s*(?=\\S)", text = doc_context$text, perl = TRUE), "match.length")
    regular_delimited <-
      paste0(split_naked_vector(doc_context$text), collapse=", ")
    vector_form <- paste0("c(",regular_delimited,")")
    insert_range <- rstudioapi::document_range(doc_context$range$start,
                                               c(doc_context$range$start[2], nchar(vector_form)+1)
                                               )
  } # parse to a vector

  if(is_horiz_vec(doc_context$text)){
    indent_context <- attr(regexpr(pattern = "^\\s*(?=\\S)", text = doc_context$text, perl = TRUE), "match.length")
    vector_form <-
      paste0(strrep(" ", indent_context), "c(",
        paste0(
          split_horiz_vec(doc_context$text),
          collapse = paste0(",\n",strrep(" ", indent_context + (doc_context$range$start[2]-1) + 2)) #2 to align for 'c('
        ),
        ")"
      )
    insert_range <-
      rstudioapi::document_range(doc_context$range$start,
                     c(doc_context$range$start[1] + length(vector_form)-1, nchar(vector_form[length(vector_form)]))
      )
   } # pivot to vertical vector

  if(is_vert_vec(doc_context$text)){
    indent_context <- attr(regexpr(pattern = "^\\s*(?=\\S)", text = doc_context$text, perl = TRUE), "match.length")
    regular_delimited <- paste0(split_vert_vec(doc_context$text), collapse = ",")
    vector_form <- paste0("c(",regular_delimited,")")
    insert_range <- rstudioapi::document_range(doc_context$range$start,
                                               c(doc_context$range$start[2], nchar(vector_form)+1)
    )

  } # pivot horiz vector

  #is_quoted_horiz_vec # pivot to quoted vertical vector

  #is_quoted_vert_vec # pivot to unqoted horizontal

  rstudioapi::modifyRange(doc_context$range, text = vector_form)
  rstudioapi::setSelectionRanges(insert_range)

  #selection_result <-
  #  tryCatch( expr = eval(parse(text = doc_context$text), envir = eval_env),
  #            error = NULL)

  #if(tibble::is_tibble(selection_result) |
  #   is.data.frame(selection_result)){ df_paste(selection_result) }


}

is_naked_vec <- function(text){
 text <- trimws(text)
 naked_vector <- "^(((\"|\')[^\'\"]+(\"|\')|\\w+)([[:blank:]])*(,)*(\n)*([[:blank:]])*)+"
 matches <- grep(x = text, pattern = naked_vector)
 length(matches) > 0 && matches == 1
}

split_naked_vector<- function(naked_vec){
  naked_vec <- trimws(naked_vec)
  trimws(strsplit(x = naked_vec, split = "(?<=(\"|\'|\\w))[[:blank:],\n]+",
           perl = TRUE)[[1]])
}

is_horiz_vec <- function(text){
  text <- trimws(text)
  horiz_vector <- "c\\(([[:blank:]]*((\"|\')[^\'\"]+(\"|\')|\\w+)[[:blank:]]*(,)*[[:blank:]]*)+\\)"
  matches <- grep(x = text, pattern = horiz_vector)
  length(matches) > 0 && matches == 1
}

split_horiz_vec <- function(horiz_vec){
  horiz_vec <- trimws(horiz_vec)
  horiz_elems <- gsub( pattern = "(^c\\(|\\)$)",
                       replacement = "",
                       x = horiz_vec)
  trimws(strsplit(x = horiz_elems, split = "(?<=(\"|\'|\\w))[[:blank:]]*,",
    perl = TRUE)[[1]])
}

is_vert_vec <- function(text){
  text <- trimws(text)
  vert_vector <- "c\\(((\\s)*((\"|\')[^\'\"]+(\"|\')|\\w+)([[:blank:]])*(,\n)*)+\\)"
  matches <- grep(x = text, pattern = vert_vector)
  length(matches) > 0 && matches == 1
}

split_vert_vec <- function(vert_vec){
  vert_vec <- trimws(vert_vec)
  vert_elems <- gsub( pattern = "(^c\\(|\\)$)",
                      replacement = "",
                      x = vert_vec)
  trimws(strsplit(vert_elems, split = "(?<=(\"|\'|\\w))[[:blank:]]*,\n", perl = TRUE)[[1]])
}
