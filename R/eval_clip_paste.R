#' Evaluate the contents of the clipboard and paste the result
#'
#' Output format is determined by the class of the result.
#'
#' @return nothing.
#' @export
eval_clip_dpasta <- function(){

  clip_source <- clipr::read_clip()

  eval_result <-
    tryCatch( expr = eval(parse(text = clip_source)),
              error = function(e) NULL)

  if (!is.null(eval_result)){
    dpasta(eval_result)
  }

  invisible(NULL)
}

#' Evaluate the contents of the clipboard and paste the result as a Tribble
#'
#' @return nothing.
#' @export
eval_clip_tribble <- function(){

  clip_source <- clipr::read_clip()

  eval_result <-
    tryCatch( expr = eval(parse(text = clip_source)),
              error = function(e) NULL)

  if (!is.null(eval_result)){
    tribble_paste(eval_result)
  }

  invisible(NULL)
}
