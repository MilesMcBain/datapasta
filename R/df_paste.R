#' df_paste
#' @description Parse the current clipboard as a table and paste in at the cursor location in data.frame format.
#' @return the text pasted to the console. Useful for testing purposes.
#' @export
#'
df_paste <- function() {

  clipboard_table <- tryCatch({
    read_clip_tbl_guess()
  }, error = function(e) {
    return(NULL)
  })
  if (is.null(clipboard_table)) {
    if (!clipr::clipr_available())
      message("Clipboard is not available. Is R running in RStudio Server or a C.I. machine?")
    else message("Could not paste clipboard as data.frame. Text could not be parsed as table.")
    return(NULL)
  }

  nspc <- .rs.readUiPref('num_spaces_for_tab')
  context <- rstudioapi::getActiveDocumentContext()
  context_row <- context$selection[[1]]$range$start["row"]
  if(all(context$selection[[1]]$range$start == context$selection[[1]]$range$end)){
    indent_context <- nchar(context$contents[context_row])
  } else{
    indent_context <- attr(regexpr("^\\s+", context$contents[context_row]),"match.length")+1 #first pos = 1 not 0
  }

  cols <- as.list(clipboard_table)
  col_types <- lapply(cols, readr::guess_parser)
  #convert the column lists to guessed types.
  cols <- mapply(function(cols, col_type){eval(parse(text = paste0("as.", col_type,"(cols)")))} , cols, col_types, SIMPLIFY = FALSE)

  ## indent by at least 12 characters
  ## nchar('data.frame(')
  ## #> 12

  charw <- max(max(nchar(names(cols))) + 3L, 12L)

  list_of_cols <- lapply(seq_along(cols), function(x) paste(pad_to(names(cols[x]), charw), "=",  cols[x]))

  #paste0 inserts its own "\n" for lists which will mess witch what we're trying to do with tortellini.
  #For now, just stripping "\n" from the output.
  list_of_cols <- lapply(list_of_cols, function(X) gsub("\n", "", X))

  output <- paste0(
    paste0("data.frame(\n",
           paste0(sapply(list_of_cols[1:(length(list_of_cols) - 1)], function(x) tortellini(x, indent_context = indent_context, add_comma = TRUE)), collapse = ""),
           paste0(sapply(list_of_cols[length(list_of_cols)], function(x) tortellini(x, indent_context = indent_context, add_comma = FALSE))),
           strrep(" ", indent_context), ")"
    ), collapse = "")


  rstudioapi::insertText(output)
  output
}

#' wrap the datpasta around itself
#' @param s input string
#' @param n number of characters for text (includes column name on line 1)
#' @param indent_context the level of indend in spaces in the current editor pane
#' @param add_comma add one final comma to the end of the wrapped column def? Useful when pasting together columns.
#' @return w wrapped string

tortellini <- function(s, n = 80, indent_context = 0, add_comma = TRUE) {

  ## if the string is less than n chars then
  ## don't worry about splitting
  if (nchar(s) > n) {

  ## determine the initial offset
  offset <- attr(regexpr("^.*?c\\(", s), "match.length") - 1L + indent_context

  ## split the string at commas
  split_s <- strsplit(s, ",")[[1]]

  ## determine the groups of strings by splitting at n chars
  ## additional 1 is for the comma to be added back
  groups <- cumsum(nchar(split_s) + 1L) %/% n

  ## paste the first group of strings back together
  wrapped_s <- paste0(strrep(" ", indent_context),
                      paste0(split_s[groups == groups[1]], collapse = ",")
               )

  ## for the remaining groups, subtract the offset
  ## from the width limit then re-calculate groupings
  split_s_rem <- split_s[groups != groups[1]]
  groups_rem <- cumsum(nchar(split_s_rem) + 1L) %/% (n - offset)
  ngroups <- length(unique(groups_rem))

  ## paste the remaining groups together
  wrapped_s[2:(ngroups+1)] <- sapply(unique(groups_rem),
                                     function(x) paste0(
                                       strrep(" ", offset),
                                       paste0(split_s_rem[groups_rem == x], collapse = ",")))

  } else { ## if no splitting is required

    wrapped_s <- s

  }

  ## append a new comma and newline to the end of each
  w <- paste0(wrapped_s, collapse = ",\n")
  w <- if(add_comma) paste0(w, ",\n") else paste0(w, "\n")

  return(w)

}
