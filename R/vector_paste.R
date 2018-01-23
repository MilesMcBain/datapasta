#' vector_paste
#'
#' @description Pastes data, either from clipboard or supplied argument, as a horizontally formatted character vector on
#' a single line. Considers `,`, `tab`, `newline` as delimiters. If a single character string is passed as an argument, it will be split to form a vector.
#' @param input_vector An input vector to be formatted for output. If supplied, no data is read from the clipboard.
#' @param output_context an optional output context that defines the output target and indentation.
#' The default behaviour is to target the rstudioapi and fall back to console if it is not available.
#' @return nothing.
#' @export
#'
vector_paste <- function(input_vector, output_context = guess_output_context()){

  vector_form <- vector_construct(input_vector, oc = output_context)

  #output depending on mode
  switch(output_context$output_mode,
         rstudioapi = rstudioapi::insertText(vector_form),
         console = cat(vector_form))
}

#' vector_format
#'
#' @description Writes data to the clipboard, either from clipboard or supplied argument. Writes a horizontally formatted character vector on
#' a single line. Considers `,`, `tab`, `newline` as delimiters. If a single character string is passed as an argument, it will be split to form a vector.
#' @param input_vector An input vector to be formatted for output. If supplied, no data is read from the clipboard.
#' @param output_context an optional output context that defines the output indentation.
#' @return nothing.
#'
#'
vector_format <- function(input_vector, output_context = console_context()){
  if(!interactive()) stop("Cannot write to clipboard in non-interactive sessions.")
  vector_form <- vector_construct(input_vector, output_context)
  clipr::write_clip(vector_form)
}

#' vector_construct
#'
#' @description Returns a formatted character string, either from clipboard or supplied argument, as a vector definition. Considers `,`, `tab`, `newline` as delimiters.
#' If a single character string is passed as an argument, it will be split to form a vector.
#' @param input_vector An input vector to be formatted for output. If supplied, no data is read from the clipboard.
#' @param oc an optional output context that defines the output indentation.
#' @return A string containing the input formatted as a vector definition.
#' @export
#'
vector_construct <- function(input_vector, oc = console_context()){

  if( missing(input_vector) ){
    input_vector <- parse_vector()
    vector_type <- readr::guess_parser(input_vector)
  }else{
    if(class(input_vector) == "character" && length(input_vector == 1)){ #Passed in a single string, going to try to break it up.
      input_vector <- parse_vector(input_vector)
      vector_type <- readr::guess_parser(input_vector)
    }else{ #You passed in a vector assume you have it delimited and set to a type you want.
      vector_type <- class(input_vector)
      input_vector <- as.character(input_vector)
      }
  }

  vector_form <- paste0(ifelse(oc$indent_head, yes = strrep(" ", oc$indent_context), no = ""), "c(",
                        paste0(
                          lapply(input_vector, render_type, vector_type),
                          collapse = ", "),
                        ")\n"
  )
  return(invisible(vector_form))
}

#' vector_paste_vertical
#'
#' @description Pastes data, either from clipboard or supplied argument, as a vertically formatted character vector over many lines. Considers `,`, `tab`, `newline` as delimiters. If a single character string is passed as an argument, it will be split to form a vector.
#' @param input_vector An input vector to be formatted for output. If supplied, no data is read from the clipboard.
#' @param output_context an optional output context that defines the output target and indentation.
#' The default behaviour is to target the rstudioapi and fall back to console if it is not available.
#' @return nothing.
#' @export
#'
vector_paste_vertical <- function(input_vector, output_context = guess_output_context()){
  vector_form <- vector_construct_vertical(input_vector, output_context)

  #output depending on mode
  switch(output_context$output_mode,
         rstudioapi = rstudioapi::insertText(vector_form),
         console = cat(vector_form))
}

#' vector_format_vertical
#'
#' @description Writes data to clipboard, either from clipboard or supplied argument, as a vertically formatted character vector over many lines.
#' Considers `,`, `tab`, `newline` as delimiters. If a single character string is passed as an argument, it will be split to form a vector.
#' @param input_vector An input vector to be formatted for output. If supplied, no data is read from the clipboard.
#' @param output_context an optional output context that defines the output target and indentation.
#' The default behaviour is to target the rstudioapi and fall back to console if it is not available.
#' @return nothing.
#'
#'
vector_format_vertical <- function(input_vector, output_context = clipboard_context()){
  if(!interactive()) stop("Cannot write to clipboard in non-interactive sessions.")
  vector_form <- vector_construct_vertical(input_vector, output_context)
  clipr::write_clip(vector_form)
}

#' vector_construct_vertical
#'
#' @description Returns a formatted string, either from clipboard or supplied argument, as a vertically formatted character vector over many lines.
#' Considers `,`, `tab`, `newline` as delimiters. If a single character string is passed as an argument, it will be split to form a vector.
#' @param input_vector An input vector to be formatted for output. If supplied, no data is read from the clipboard.
#' @param oc an optional output context that defines the output target and indentation.
#' The default behaviour is to target the rstudioapi and fall back to console if it is not available.
#' @return a string containing the input formatted as a vector definition.
#' @export
#'
vector_construct_vertical <- function(input_vector, oc = console_context()){
  if( missing(input_vector) ){
    input_vector <- parse_vector()
    vector_type <- readr::guess_parser(input_vector)
  }else{
    if(class(input_vector) == "character" && length(input_vector == 1)){ #Passed in a single string, going to try to break it up.
      input_vector <- parse_vector(input_vector)
      vector_type <- readr::guess_parser(input_vector)
    }else{ #You passed in a vector assume you have it delimited and set to a type you want.
      vector_type <- class(input_vector)
      input_vector <- as.character(input_vector)
    }
  }

  vector_form <- paste0(ifelse(oc$indent_head, yes = strrep(" ", oc$indent_context), no = ""), "c(",
                        paste0(
                          lapply(input_vector, render_type, vector_type),
                          collapse = paste0(",\n",strrep(" ", oc$indent_context + 2)) #2 to align for 'c('
                        ),
                        ")\n"
  )
  return(invisible(vector_form))
}

#' parse_vector
#'
#' @description Pastes data from clipboard as a vertically formatted character vector on
#' multiple lines. One line is used per element. Considers `,`, `tab`, `newline` as delimiters.
#' @param input_vector an optional character vector to attempt to break up, and escape.
#' @return A vector parsed from the clipboard as ether a character string or a
#' character vector. The type attribute contains the type guessed by `readr`.
#'
#'
parse_vector <- function(input_vector){
  if(missing(input_vector)){
    input_vector <- tryCatch({clipr::read_clip()},
                                 error = function(e) {
                                     return(NULL)
                                 })
    if(is.null(input_vector)){
        if(!clipr::clipr_available()) message(.global_datapasta_env$no_clip_msg)
        else message("Could not paste clipboard as a vector. Text could not be parsed.")
        return(NULL)
    }
  }

  if(length(input_vector) == 1){
    input_vector <- trimws(unlist(
      strsplit(
        x = input_vector,
        split = "\t|,",
        perl= TRUE))
    )
  }else{
    input_vector <- trimws(input_vector)
  }
  #strip ending comma delim
  input_vector <- ifelse(grepl(pattern = ",$", input_vector),
                         yes = substr(input_vector, 1, nchar(input_vector)-1),
                         no = input_vector)
  #strip outer quotes
  input_vector <- ifelse(grepl(pattern = "(^\".*\"$)|(^\'.*\'$)", input_vector),
                         yes = substr(input_vector, 2, nchar(input_vector)-1),
                         no = input_vector)
  input_vector
}
