#' df_paste
#' @description Parse the current clipboard as a table and paste in at the cursor location in data.frame format.
#' @return nothing.
#' @export
#'
df_paste <- function() {

  clipboard_table <- tryCatch({
    datapasta:::read_clip_tbl_guess()
  }, error = function(e) {
    return(NULL)
  })
  if (is.null(clipboard_table)) {
    if (!clipr::clipr_available())
      message("Clipboard is not available. Is R running in RStudio Server or a C.I. machine?")
    else message("Could not paste clipboard as data.frame. Text could not be parsed as table.")
    return(NULL)
  }

  cols <- as.list(clipboard_table)

  list_of_cols <- lapply(seq_along(cols), function(x) paste(names(cols[x]), "=",  cols[x]))

  output <- paste0(paste0("data.frame(\n",
                   paste0(sapply(list_of_cols[1:length(list_of_cols) - 1], function(x) x), ",\n", collapse = ""),
                   paste0(paste0(list_of_cols[length(list_of_cols)]), "\n)", collapse = "")), collapse = "")

  rstudioapi::insertText(output)

}
