#' vector_paste
#'
#' @description Pastes data from clipboard as a horizontally formatted character vector on
#' a single line. Considers , | tab newline as delimeters.
#' @param input_vector An input vector to be formatted for output. If supplied, no data is read from the clipboard.
#' @return nothing.
#' @export
#'
vector_paste <- function(input_vector){

  if( missing(input_vector) ){
    input_vector <- parse_vector()
    vector_type <- readr::guess_parser(input_vector)
  }else{
    vector_type <- class(input_vector)
    input_vector = as.character(input_vector)
  }
  oc <- get_output_context()

  vector_form <- paste0("c(",
    paste0(
      lapply(input_vector, render_type, vector_type),
      collapse = ", "),
    ")"
  )

  #output depending on mode
  switch(oc$output_mode,
         rstudioapi = rstudioapi::insertText(vector_form),
         console = cat(vector_form))
  return(invisible(vector_form))
}

#' vector_paste_vertical
#'
#' @description Pastes data from clipboard as a vertically formatted character vector on
#' a multiple lines. One line is used per element. Considers , | tab newline as delimeters.
#' @param input_vector An input vector to be formatted for output. If supplied, no data is read from the clipboard.
#' @return nothing.
#' @export
#'
vector_paste_vertical <- function(input_vector){
  if( missing(input_vector) ){
    input_vector <- parse_vector()
    vector_type <- readr::guess_parser(input_vector)
  }else{
    vector_type <- class(input_vector)
    input_vector = as.character(input_vector)
  }

  # Determine output. Either rstudioapi or console
  oc <- get_output_context()

  vector_form <- paste0("c(",
                    paste0(
                      lapply(input_vector, render_type, vector_type),
                      collapse = paste0(",\n",strrep(" ", oc$indent_context + 2)) #2 to align for 'c('
                    ),
                    ")"
                  )
  #output depending on mode
  switch(oc$output_mode,
         rstudioapi = rstudioapi::insertText(vector_form),
         console = cat(vector_form))
  return(invisible(vector_form))
}

#' parse_vector
#'
#' @description Pastes data from clipboard as a vertically formatted character vector on
#' a multiple lines. One line is used per element. Considers , | tab newline as delimeters.
#'
#' @return A vector parsed from the clipboard as ether a character string or a
#' character vector. The type attribute contains the type guessed by `readr`.
#'
#'
parse_vector <- function(){
  clipboard_string <- tryCatch({clipr::read_clip()},
                               error = function(e) {
                                   return(NULL)
                               })
  if(is.null(clipboard_string)){
      if(!clipr::clipr_available()) message("Clipboard is not available. Is R running in RStudio Server or a C.I. machine?")
      else message("Could not paste clipboard as a vector. Text could not be parsed.")
      return(NULL)
  }

  if(length(clipboard_string) == 1){
    input_vector <- unlist(
      strsplit(
        x = clipboard_string,
        split = "\t|,|\\|",
        perl= TRUE)
    )
  }else{
    input_vector <- clipboard_string
  }
  input_vector
}

