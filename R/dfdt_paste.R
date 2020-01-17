#' df_paste
#' @description Parse either: the current clipboard, or a supplied argument, as a table and paste in at the cursor location in data.frame format.
#' @param input_table an optional input tibble or data.frame to format.
#' @param output_context an optional output context that defines the target and indentation.
#' The default behaviour is target the rstudioapi and fall back to console if it is not available.
#' @return nothing.
#' @export
#'
df_paste <- function(input_table, output_context = guess_output_context()){
  output <- dfdt_construct(input_table, oc = output_context, class = "data.frame")

  #output depending on mode
  switch(output_context$output_mode,
         rstudioapi = rstudioapi::insertText(output),
         console = cat(output))
}

#' dt_paste
#' @description Parse either: the current clipboard, or a supplied argument, as a table and paste in at the cursor location in data.table format.
#' @param input_table an optional input tibble or data.frame to format.
#' @param output_context an optional output context that defines the target and indentation.
#' The default behaviour is target the rstudioapi and fall back to console if it is not available.
#' @return nothing.
#' @export
#'
dt_paste <- function(input_table, output_context = guess_output_context()){
  output <- dfdt_construct(input_table, oc = output_context, class = "data.table")

  #output depending on mode
  switch(output_context$output_mode,
         rstudioapi = rstudioapi::insertText(output),
         console = cat(output))
}

#' df_format
#' @description Parse the current clipboard as a table and paste to the clipboard in data.frame format.
#' @param input_table an optional input tibble or data.frame to format.
#' @param output_context an optional output context that defines the target and indentation.
#' @return nothing.
#' @export
#'
df_format <- function(input_table, output_context = clipboard_context()){
  if(!interactive()) stop("Cannot write to clipboard in non-interactive sessions.")
  output <- dfdt_construct(input_table, oc = output_context, class = "data.frame")
  clipr::write_clip(output)
}

#' dt_format
#' @description Parse the current clipboard as a table and paste to the clipboard in data.table format.
#' @param input_table an optional input tibble or data.frame to format.
#' @param output_context an optional output context that defines the target and indentation.
#' @return nothing.
#' @export
#'
dt_format <- function(input_table, output_context = clipboard_context()){
  if(!interactive()) stop("Cannot write to clipboard in non-interactive sessions.")
  output <- dfdt_construct(input_table, oc = output_context, class = "data.table")
  clipr::write_clip(output)
}

#' dfdt_construct
#' @description Parse the current clipboard as a table and return in data.frame format.
#' @param input_table an optional R object to parse instead of the clipboard.
#' @param oc an optional output context that defines the target and indentation.
#' @param class either data.frame or data.table.
#' @return a character string containing the input formatted as a data.frame definition.
#' @export
#'
dfdt_construct <- function(input_table, oc = console_context(), class = NULL) {

  if(missing(input_table)){
    input_table <- read_clip_tbl_guess()

    if (is.null(class)) stop("Requires either \"data.frame\" or \"data.table\" class")

     if(is.null(input_table)){
      message("Could not paste clipboard as data.frame/data.table. Text could not be parsed as table.")
      return(NULL)
     }

    #Parse data types from string using readr::parse_guess
    col_types <- lapply(input_table, readr::guess_parser, guess_integer = TRUE)
    cols <- as.list(input_table)
  }else{
    if(!is.data.frame(input_table) && !is_tibble(input_table)){
      message("Could not format input_table as table. Unexpected class.")
      return(NULL)
    }
    if(nrow(input_table) >= .global_datapasta_env$max_rows){
      message(paste0("Supplied large input_table (>=", .global_datapasta_env$max_rows ," rows). Was this a mistake? Use dp_set_max_rows(n) to increase the limit."))
      return(NULL)
    }
    #If data.drame (vs. data.table), keep meaningful rownames (to return later)
    if(methods::is(input_table, "data.frame") & !methods::is(input_table, "data.table") &
        !all(rownames(input_table) == seq(nrow(input_table)))){
      row_names <- rownames(input_table)
    }
    col_types <- lapply(input_table, base::class) # prevent clobbering by local class variable
    #Store types as characters so the char lengths can be computed
    #Dont't fix the names with prefixes!
    input_table <- as.data.frame(lapply(input_table, as.character),
                                 stringsAsFactors = FALSE,
                                 check.names = FALSE)
    #Store types as characters so the char lengths can be computed
    cols <- as.list(input_table)
  }

  contains_chars <- any(col_types == "character") #we'll need to add stringsAsFactors=FALSE if so.

  #Extract column names, surrounding with backticks if they do not start with a latin character
  col_names_valid <- ifelse(make.names(names(cols)) == names(cols), names(cols), paste0("`", names(cols), "`"))



  #Set the column name width
  ##Work out lengths of needed args
  row_names_length <- if (exists("row_names")) nchar("row.names") else 0
  strings_as_factors_length <-
    if (contains_chars && class == "data.frame") nchar("stringsAsFactors") else 0
  check_names_length <-
    if (any(col_names_valid != names(cols))) nchar("check.names") else 0
  ##compare lengths of col names and needed args, choose min = 10,
  ##max = longest col name or arg + 2 for an indent
  charw <- max(max(nchar(col_names_valid)),
               row_names_length,
               strings_as_factors_length,
               check_names_length,
               10L) + 2L

  #Generate lists of data ready for formatting
  list_of_cols <- lapply(which(col_types != "factor"), function(x) list(name = paste0(pad_to(col_names_valid[x], charw)),
                                                                        call = "= c(",
                                                                        data = unlist(lapply(cols[[x]], render_type, col_types[[x]])),
                                                                        close = ")"))

  #Handle the factor columns specially.
  if(any(col_types == "factor")){
    list_of_factor_cols <-
      lapply(which(col_types == "factor"), function(x) list(name = pad_to(col_names_valid[x], charw),
                                                            call = "= as.factor(c(",
                                                            data = unlist(lapply(cols[[x]], render_type, col_types[[x]])),
                                                            close = "))"))

    list_of_cols <- c(list_of_cols, list_of_factor_cols)
    names(list_of_cols) <- names(cols)
  }

  output <-
    paste0(
      c(paste0(ifelse(oc$indent_head,
                         yes = strrep(" ", oc$indent_context),
                         no = ""),
                  ifelse(class == "data.frame",
                         yes = "data.frame(",
                         no = "data.table::data.table(")
                  ),
           if (strings_as_factors_length > 0)
             tortellini(list(name = pad_to("stringsAsFactors", charw),
                             call = "= ",
                             data = "FALSE",
                             close =  ""),
                        indent_context = oc$indent_context,
                        add_comma = TRUE),
           if (check_names_length > 0)
              tortellini(list(name = pad_to("check.names", charw),
                              call = "= ",
                              data = "FALSE",
                              close =  ""),
                        indent_context = oc$indent_context,
                        add_comma = TRUE),
           if (row_names_length > 0)
              tortellini(list(name = pad_to("row.names", charw),
                              call = "= c(",
                              data = lapply(row_names, render_type, rep("character", length(row_names))),
                              close =  ")"),
                              indent_context = oc$indent_context,
                              add_comma = TRUE),
           if(length(list_of_cols) > 1)
              paste0(sapply(list_of_cols[seq_len(length(list_of_cols) - 1)],
                          function(x) tortellini(x,
                                                 indent_context = oc$indent_context,
                                                 add_comma = TRUE)),
                  collapse = "\n"),
           sapply(list_of_cols[length(list_of_cols)],
                  function(x) tortellini(x,
                                        indent_context = oc$indent_context,
                                        add_comma = FALSE)),
           paste0(strrep(" ", oc$indent_context),")")
          ),
      collapse = "\n")

  return(output)
}

#' wrap the datapasta around itself
#' @param col_struct input structure - a split apart column definition
#' @param defn_width total number of characters in a line (includes column name and indent on line 1)
#' @param indent_context the level of indent in spaces in the current editor pane
#' @param add_comma add one final comma to the end of the wrapped column def? Useful when pasting together columns.
#' @return w wrapped string

tortellini <- function(col_struct, defn_width = 80, indent_context = 0, add_comma = TRUE) {

  split_s <- list(paste(col_struct$name, col_struct$call),
         col_struct$data,
         col_struct$close)

  joined_s <- paste0(paste0(split_s[[1]],
                            paste0(split_s[[2]], collapse = ", ")),
                     split_s[[3]])

  ## calculate indent context

  ## if the string is less than n chars then
  ## don't worry about splitting
  if ((nchar(joined_s) + indent_context) > defn_width) {

  ## determine the initial offset
  offset <- nchar(split_s[[1]]) + indent_context

  ## try to fit the whole defn within n chars, but draw a line at data with of 20
  ## so we have something to work with
  group_length <- max(defn_width - offset, 20)

  ## determine the groups of strings by splitting at n chars
  ## additional 2 is for the comma space to be added back
  groups <- (indent_context + nchar(split_s[[1]]) + cumsum(nchar(split_s[[2]]) + 2L)) %/% group_length

  ## paste groups together
  wrapped_data <- sapply(unique(groups),
                      function(x)
                        paste0(split_s[[2]][groups == x], collapse = ","))

  ## for group 1 add the definition call
  ## needs whitespace trimmed because it had indent_context added which needs to be
  ## added at front of line
  wrapped_data[1] <- paste0(strrep(" ", indent_context), split_s[[1]], trimws(wrapped_data[1]))

  ## for groups after group 1, add the offset
  wrapped_data[2:length(wrapped_data)] <- paste0(strrep(" ", offset), wrapped_data[2:length(wrapped_data)])

  ## join parts together for final output
  wrapped_s <- paste0(paste0(wrapped_data, collapse = ",\n"),
         split_s[3])

  } else { ## if no splitting is required

    wrapped_s <- joined_s

  }

  ## append a new comma and newline to the end of each
  w <- paste0(wrapped_s, collapse = ",\n")
  w <- if(add_comma) paste0(w, ",") else w

  return(w)

}
