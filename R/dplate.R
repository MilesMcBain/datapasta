#' dplate
#'
#' @param input a vector, data.frame, or tibble
#'
#' @description Plates up input for presentation in mardkown as a preformatted chunk and inserts it onto the clipboard. Ready for pasting to Stack Overflow or Github.
#' @return nothing
#'
#' @examples
dplate <- function(input){
  if(tibble::is_tibble(input)){
    tribble_format(input, output_context = markdown_context())
  }else if(is.data.frame(input)){
    df_format(input, output_context = markdown_context())
  }else if(is.vector(input)){
    vector_format(input, output_context = markdown_context())
  }
}
