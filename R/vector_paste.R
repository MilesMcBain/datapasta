#' vector_paste
#'
#' @description Pastes data from clipboard formatted as a character vector. Considers , | tab newline as delimeters
#'
#' @return nothing.
#' @export
#'
vector_paste <- function(){
  clipboard_string <- clipr::read_clip()
  nspc <- .rs.readUiPref('num_spaces_for_tab')
  context <- rstudioapi::getActiveDocumentContext()
  context_row <- context$selection[[1]]$range$end["row"]
  indent_context <- nchar(context$contents[context_row])

  clipboard_vector <- unlist(
    strsplit(
      x = clipboard_string,
      split = "\t|,|\\|",
      perl= TRUE)
    )
  
  vector_type <- readr::guess_parser(clipboard_vector)

  vector_form <- paste0("c(",
    paste0(
      ifelse(is.na(clipboard_vector), yes = "NA", no = paste0('"',clipboard_vector,'"'))
      , collapse = ", "),
    ")"
  )
  rstudioapi::insertText(vector_form)
}
