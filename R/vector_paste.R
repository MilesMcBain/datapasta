#' vector_paste
#'
#' @description Pastes data from clipboard formatted as a character vector. Considers , | tab newline as delimeters
#'
#' @return nothing.
#' @export
#'
vector_paste <- function(){
  clipboard_table <- clipr::read_clip()
  nspc <- .rs.readUiPref('num_spaces_for_tab')
  context <- rstudioapi::getActiveDocumentContext()
  context_row <- context$selection[[1]]$range$end["row"]
  indent_context <- nchar(context$contents[context_row])


  studioapi::insertText("tba")
}
