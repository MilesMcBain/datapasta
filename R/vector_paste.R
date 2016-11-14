#' vector_paste
#'
#' @description Pastes data from clipboard formatted as a character vector. Considers , | tab newline as delimeters
#'
#' @return nothing.
#' @export
#'
vector_paste <- function(){

  clipboard_vector <- parse_vector()
  vector_type <- attr(clipboard_vector, "type")

  vector_form <- paste0("c(",
    paste0(
      #ifelse(is.na(clipboard_vector), yes = "NA", no = paste0('"',clipboard_vector,'"'))
      lapply(clipboard_vector, render_type, vector_type), 
      collapse = ", "),
    ")"
  )
  rstudioapi::insertText(vector_form)
}

vector_paste_vertical <- function(){
  clipboard_vector <- parse_vector()
  vector_type <- attr(clipboard_vector, "type")
  
  nspc <- .rs.readUiPref('num_spaces_for_tab')
  context <- rstudioapi::getActiveDocumentContext()
  context_row <- context$selection[[1]]$range$end["row"]
  indent_context <- nchar(context$contents[context_row])
  
  vector_form <- paste0("c(",
                    paste0(
                      #ifelse(is.na(clipboard_vector), yes = "NA", no = paste0('"',clipboard_vector,'"'))
                      lapply(clipboard_vector, render_type, vector_type), 
                      collapse = paste0(",\n",strrep(" ", indent_context + nspc))
                    ),
                    ")"
                  )     
  rstudioapi::insertText(vector_form)
}

parse_vector <- function(){
  clipboard_string <- clipr::read_clip()
  if(length(clipboard_string == 1)){
    clipboard_vector <- unlist(
      strsplit(
        x = clipboard_string,
        split = "\t|,|\\|",
        perl= TRUE)
    )
  }else{
    clipboard_vector <- clipboard_string
  }
  vector_type <- readr::guess_parser(clipboard_vector)
  
  attr(clipboard_vector, "type") <- vector_type
  clipboard_vector
}

render_type <- function(char_vec, type_str){
    output <- switch(type_str,
                     "integer" = as.integer(char_vec),
                     "double" = as.double(char_vec),
                     "logical" = as.logical(char_vec),
                     "character" = ifelse(is.na(char_vec), yes = "NA", no = paste0('"',char_vec,'"')),
                     paste0('"',char_vec,'"')
    )    
    output
}
