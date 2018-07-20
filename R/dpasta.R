#' dmdclip
#'
#' @param input a vector, data.frame, or tibble
#'
#' @description Formats input for presentation in markdown as a preformatted chunk and inserts it onto the clipboard. Ready for pasting to Stack Overflow or Github.
#' @return nothing
#'
#' @export
dmdclip <- function(input){
  if(tibble::is_tibble(input)){
    tribble_format(input, output_context = markdown_context())
  }else if(is.data.frame(input)){
    df_format(input, output_context = markdown_context())
  }else if(is.vector(input)){
    vector_format(input, output_context = markdown_context())
  }
}

#' dpasta
#'
#' @param input a vector, data.frame, or tibble
#'
#' @description Formats input and inserts at either the current cursor or the console.
#' @return nothing
#'
#' @export
dpasta <- function(input){
  if(tibble::is_tibble(input)){
    tribble_paste(input, output_context = guess_output_context())
  }else if(is.data.frame(input)){
    df_paste(input, output_context = guess_output_context())
  }else if(is.vector(input)){
    vector_paste(input, output_context = guess_output_context())
  }
}
