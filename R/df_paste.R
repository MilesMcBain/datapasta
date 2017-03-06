#' df_paste
#' @description Parse the current clipboard as a table and paste in at the cursor location in data.frame format.
#' @return the text pasted to the console. Useful for testing purposes.
#' @export
#'
df_paste <- function(input_table) {

  if(missing(input_table)){
    input_table <- tryCatch({read_clip_tbl_guess()},
                            error = function(e) {
                              return(NULL)
                            })

    if(is.null(input_table)){
      if(!clipr::clipr_available()) message("Clipboard is not available. Is R running in RStudio Server or a C.I. machine?")
      else message("Could not paste clipboard as data.frame. Text could not be parsed as table.")
      return(NULL)
    }
    #Parse data types from string using readr::parse_guess
    col_types <- lapply(input_table, readr::guess_parser)
    cols <- as.list(input_table)
  }else{
    if(!is.data.frame(input_table) && !tibble::is_tibble(input_table)){
      message("Could not format input_table as table. Unexpected class.")
      return(NULL)
    }
    if(nrow(input_table) >= 200){
      message("Supplied large input_table (>= 200 rows). Was this a mistake? Large tribble() output is not supported.")
      return(NULL)
    }
    col_types <- lapply(input_table, class)
    #Store types as characters so the char lengths can be computed
    input_table <- as.data.frame(lapply(input_table, as.character), stringsAsFactors = FALSE)
    #Store types as characters so the char lengths can be computed
    cols <- as.list(input_table)
  }

  nspc <- .rs.readUiPref('num_spaces_for_tab')
  context <- rstudioapi::getActiveDocumentContext()
  context_row <- context$selection[[1]]$range$start["row"]
  if(all(context$selection[[1]]$range$start == context$selection[[1]]$range$end)){
    indent_context <- nchar(context$contents[context_row])
  } else{
    indent_context <- attr(regexpr("^\\s+", context$contents[context_row]),"match.length")+1 #first pos = 1 not 0
  }

  #Set the column name width
  charw <- max(max(nchar(names(cols))) + 3L, 12L)

  list_of_cols <- lapply(which(col_types != "factor"), function(x) paste(pad_to(names(cols[x]), charw), "=",
                                                            paste0("c(",
                                                              paste0( unlist(lapply(cols[[x]], render_type, col_types[[x]])), collapse=", "),
                                                                ")"
                                                            )
                                                       )
                         )

  #Handle the factor columns specially.
  if(any(col_types == "factor")){
  list_of_factor_cols <-
    lapply(which(col_types == "factor"), function(x) paste(pad_to(names(cols[x]), charw), "=",
                                                       paste0("as.factor(c(",
                                                          paste0( unlist(lapply(cols[[x]], render_type, col_types[[x]])), collapse=", "),
                                                            "))"
                                                          )
                                                      )
    )
    list_of_cols <- c(list_of_cols, list_of_factor_cols)
    names(list_of_cols) <- names(cols)
  }



  #paste0 inserts its own "\n" for lists which will mess with what we're trying to do with tortellini.
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
