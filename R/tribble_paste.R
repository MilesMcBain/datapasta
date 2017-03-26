globalVariables(c(".rs.readUiPref",".global_datapasta_env"), "datapasta") #ignore this function in R CMD checks, since it is part of RStudio runtime
.global_datapasta_env <- new.env(parent=emptyenv())
.global_datapasta_env$decimal_mark <- "."
.global_datapasta_env$max_rows <- 200

#' tribble_paste
#' @description Parse the current clipboard as a table, or use the table argument supplied, and paste in at the cursor location in tribble format.
#' @param input_table an optional input `data.frame`. If `input_table` is supplied, then nothing is read from the clipboard.
#' @param output_context an optional output context that defines the target and indentation. Default is to guess between rstudio and console.
#' Table is output as `tribble()` call. Useful for creating reproducible examples.
#' @return Nothing.
#' @export
#'
tribble_paste <- function(input_table, output_context = guess_output_context()){
  output <- tribble_construct(input_table, oc = output_context)

  switch(output_context$output_mode,
         rstudioapi = rstudioapi::insertText(output),
         console = cat(output))
}

#' tribble_format
#' @description Parse the current clipboard as a table, or use the table argument supplied, and paste to the clipboard in tribbble format.
#' @param input_table an optional input `data.frame`. If `input_table` is supplied, then nothing is read from the clipboard.
#' @param output_context an optional output context that defines the target and indentation. Default is console.
#' Table is output as `tribble()` call. Useful for creating reproducible examples.
#' @return Nothing.
tribble_format <- function(input_table, output_context = console_context()){
  output <- tribble_construct(input_table, oc = output_context)
  clipr::write_clip(output)
}

#' tribble_construct
#' @description Parse the current clipboard as a table, or use the table argument supplied, and return as a character string.
#' @param input_table an optional input `data.frame`. If `input_table` is supplied, then nothing is read from the clipboard.
#' @param oc an optional output context that defines the target and indentation. Default is console.
#' Table is output as `tribble()` call. Useful for creating reproducible examples.
#' @return The parsed table text.
#' @export
#'
tribble_construct <- function(input_table, oc = console_context()){
  # Determine input. Either clipboard or supplied table.
  if(missing(input_table)){
    input_table <- tryCatch({read_clip_tbl_guess()},
                            error = function(e) {
                              return(NULL)
                            })

    if(is.null(input_table)){
      if(!clipr::clipr_available()) message("Clipboard is not available. Is R running in RStudio Server or a C.I. machine?")
      else message("Could not paste clipboard as tibble. Text could not be parsed as table.")
      return(NULL)
    }
    #Parse data types from string using readr::parse_guess
    input_table_types <- lapply(input_table, readr::guess_parser)
  }else{
    if(!is.data.frame(input_table) && !tibble::is_tibble(input_table)){
      message("Could not format input_table as table. Unexpected class.")
      return(NULL)
    }
    if(nrow(input_table) >= .global_datapasta_env$max_rows){
      message(paste0("Supplied large input_table (>= ",.global_datapasta_env$max_rows," rows). Was this a mistake? Use dp_set_max_rows(n) to increase the limit."))
      return(NULL)
    }
    input_table_types <- lapply(input_table, class)
    #Store types as characters so the char lengths can be computed
    input_table <- as.data.frame(lapply(input_table, as.character), stringsAsFactors = FALSE)
  }

  #Find the max length of data as string in each column
  col_widths <- mapply(input_table,
                       FUN =
                         function(df_col, df_col_type){
                           max( vapply(X = df_col,
                                       FUN = nchar_type,
                                       FUN.VALUE = numeric(1),
                                       df_col_type = df_col_type
                                ),
                           na.rm = TRUE
                           )
                         },
                       df_col_type = input_table_types

  )
  #Set the column width depending on the max length of data as string or the header, whichever is longer.
  col_widths <- mapply(max,
                       col_widths,
                       nchar(names(input_table))+1) #+1 for "~"

  #Header
  header <- paste0(ifelse(oc$indent_head, yes = strrep(" ", oc$indent_context), no = ""), "tibble::tribble(\n")

  #Column names
  names_row <- paste0(
                  paste0(strrep(" ",oc$indent_context+oc$nspc),
                      paste0(
                        paste0(
                          mapply(
                            pad_to,
                            paste0("~",names(input_table)),
                            col_widths
                          ),
                          ","
                        ),
                        collapse = " "
                      )
                    ), "\n"
                )


  #Write correct data types
  body_rows <- lapply(X = as.data.frame(t(input_table), stringsAsFactors = FALSE),
                      FUN =
                        function(col){
                          paste0(strrep(" ",oc$indent_context+oc$nspc),
                            paste0(
                                   paste0(
                                     mapply(
                                       render_type_pad_to,
                                       col,
                                       input_table_types,
                                       col_widths
                                     ),
                                     ","
                                   ),
                                   collapse = " "
                            ),
                            "\n",
                            collapse = ""
                          )

                        }
  )




  #Need to remove the final comma that will break everything.
  body_rows <- paste0(as.vector(body_rows),collapse = "")
  body_rows <- gsub(pattern = ",\n$", replacement = "\n", x = body_rows)

  #Footer
  footer <- paste0(strrep(" ",oc$indent_context+oc$nspc),")")
  output <- paste0(header, names_row, body_rows, footer)

  return(invisible(output))
}


#' nchar_type
#'
#' @param df_col_row a character string
#' @param df_col_type the type the string will be converted to.
#'
#' @return The number of characters wide this data would be in when rendered in text
nchar_type <- function(df_col_row, df_col_type){
  n_chars <- nchar(df_col_row)
  add_chars <- switch(df_col_type,
                      "integer" = 1, #for the "L",
                      "character" = 2 + length(gregexpr(pattern = "(\"|\')", text = df_col_row)[[1]]), #2 for outer quotes +1 "\" for each quote in string
                      0) #0 for other types
  return(n_chars + add_chars)

}

#' pad_to
#' @description Left pad string to a certain size. A helper function for getting spacing in table correct.
#' @param char_vec character vector.
#' @param char_length length to pad to.
#'
#' @return char_vec left-padded with spaces to char_length.
#'
pad_to <-function(char_vec, char_length){
  paste0(strrep(" ",char_length - nchar(char_vec)),char_vec)
}

#' render_type
#'
#' @description Renders a character vector as R types for pasting into Rstudio.
#' Strings are quoted. Numbers, NA, logicals etc are not.
#'
#' @param char_vec a chracter vector containing text to be rendered as the type indicated by type_str
#' @param char_type a string describing the type of char_vec
#'
#' @return A vector parsed from the clipboard as ether a character string or a
#' character vector. The type attribute contains the type guessed by `readr`.
#'
#'
render_type <- function(char_vec, char_type){
  if(is.na(char_vec)){
    output <- switch(char_type,
                     "integer" = "NA",
                     "double" = "NA",
                     "logical" = "NA",
                     "numeric" = "NA",
                     "character" = "NA",
                     "NA"
    )
  }else{
    output <- switch(char_type,
                     "integer" = paste0(as.integer(char_vec),"L"),
                     "double" = as.double(char_vec),
                     "number" = readr::parse_number(char_vec, locale = readr::locale(decimal_mark = .global_datapasta_env$decimal_mark)),
                     "numeric" = as.double(char_vec),
                     "logical" = as.logical(char_vec),
                     "factor" = ifelse(nchar(char_vec)!=0, paste0('"',char_vec,'"'), "NA"),
                     "character" = ifelse(nchar(char_vec)!=0, paste0('"',escape_chars(char_vec),'"'), "NA"),
                     "list" = char_vec,
                     paste0('"',char_vec,'"')
    )
  }
  output
}


#' render_type_pad_to
#' @description Based on a type and length, render a character string as the type in text.
#' Pad to the desired length.
#'
#' @param char_vec a character vector
#' @param char_type a string type from readr::guess_parser
#' @param char_length a string length to pad to.
#'
#' @return a string containing the representation of char_vec as char_type in the RStudio source editor,
#' left-padded with spaces to char_length.
#'
render_type_pad_to <- function(char_vec, char_type, char_length){
    pad_to(render_type(char_vec, char_type), char_length)
}

#' guess_sep
#'
#' @param char_vec a table from the clipboard in character vector form.
#'
#' @description Guesses the seprator based on a simple heuristic over the first 10 or less rows:
#' The separator chosen is the one that leads to the most columns, whilst parsing the same number of columns for each line (var=0).
#' The guessing algorithm ignores blank lines - which are lines that contain only the separator.
#' Options are in c(",","\\t","\\|,;")
#
#'
#' @return the separator selected to parse char_vec as a table
#'
guess_sep <- function(char_vec){
  candidate_seps <- c(",","\t","\\|",";")
  table_sample <- char_vec[1:min(length(char_vec),10)]

  #handle seps at end of line. A sep at the end of line is effectively an NA in the last column.
  table_sample <- gsub(",$", ", ", table_sample)

 splits <-
      lapply(
        lapply(candidate_seps,
           function(sep, table_sample){
            blank_lines <- grepl(paste0("^",sep,"*$"), table_sample)
            filtered_sample <- table_sample[!blank_lines]
            line_splits <- strsplit(filtered_sample, split = sep)
            split_lengths <- lapply(X = line_splits, FUN = length)
           },
           table_sample
        ),
      unlist)
 sep_scores <- ( !as.logical( unlist( lapply(splits, stats::var) ) ) ) * unlist( lapply(splits, max) )
 #Seps that have cols with any variance get score 0.
 sep <- candidate_seps[which.max(sep_scores)]
 if(sep == "\\|") sep <- "|"
 sep
}

#' read_clip_table_guess
#'
#' @param x contents of the clipboard
#' @param ... arguments passed to read.table
#' @description Similar to read_clip_tbl from clipr,
#' however it will error if there are less than 2 rows
#' and it tries to guess the separator.
#'
#' @return a parsed table from the clipboard. Separator is guessed.
read_clip_tbl_guess <- function (x = clipr::read_clip(), ...)
{
  if (is.null(x))
    return(NULL)
  if(length(x) < 2)  #You're just a header row, get outta here!
    return(NULL)
  .dots <- list(...)
  .dots$file <- textConnection(x)
  if (is.null(.dots$header))
    .dots$header <- TRUE
  if (is.null(.dots$sep)){
    .dots$sep <- guess_sep(x)
  }
  if (is.null(.dots$colClasses))
    .dots$colClasses <- "character"
  if (is.null(.dots$stringsAsFactors))
    .dots$stringsAsFactors <- FALSE
  if (is.null(.dots$na.strings))
    .dots$na.strings <- c("NA", "")
  if (is.null(.dots$strip.white))
    .dots$strip.white <- TRUE
    .dots$quote <-  ""
  do.call(utils::read.table, args = .dots)
}

#' dp_set_decimal_mark
#'
#' @param mark The decimal mark to use when parsing "number" type data, as guessed by readr::guess_parser.
#' @description A function to optionally set the decimal mark if in a locale where it is not `.`. Will allow "3,14" to be parsed as 3.14, normally would be parsed as 314.
#' Will also handle spaces in numbers.
#'
#' @return NULL.
#' @export
dp_set_decimal_mark <- function(mark){
  .global_datapasta_env$decimal_mark <- mark
  invisible(NULL)
}

#' dp_set_max_rows
#'
#' @param num_rows The number of rows of an input at which any of tribble_construct() or df_contruct() will abort parsing. Datapasta is untested on large tables. Use at own risk.
#'
#' @return NULL
#' @export
dp_set_max_rows <- function(num_rows){
  .global_datapasta_env$max_rows <- num_rows
  invisible(NULL)
}

#' guess_output_context
#'
#' @description Return the a list containing the guessed output target context, either rstudio or the console.
#'
#' @return a list containint the output target, space size of indent, and number of indents at context.
guess_output_context <- function(){
  if(requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()){
    output_context <- rstudio_context()
  }else{
    # rstudioapi unavailable. fallback to console
    output_context <- console_context()
  }
  output_context
}

#' @rdname custom_context
#' @export
#'
clipboard_context <- function(){
  output_context <- list(output_mode = "clipboard", nspc = 2, indent_context = 0, indent_head = FALSE)
  output_context
}

#' @rdname custom_context
#' @export
#'
rstudio_context <- function(){
  output_context <- list()
  output_context$indent_head <- FALSE #head already at cursor
  output_context$output_mode <- "rstudioapi"
  output_context$nspc <- .rs.readUiPref('num_spaces_for_tab')
  context <- rstudioapi::getActiveDocumentContext()
  context_row <- context$selection[[1]]$range$start["row"]
  if(all(context$selection[[1]]$range$start == context$selection[[1]]$range$end)){
    output_context$indent_context <- nchar(context$contents[context_row])
  } else{
    output_context$indent_context <- attr(regexpr("^\\s+", context$contents[context_row]),"match.length")+1 #first pos = 1 not 0
  }
  output_context
}

#' @rdname custom_context
#' @export
#'
console_context <- function(){
  output_context <- list(output_mode = "console", nspc = 2, indent_context = 0, indent_head = FALSE)
  output_context
}

#' @rdname custom_context
#' @export
#'
markdown_context <- function(){
  output_context <- list(output_mode = "console", nspc = 2, indent_context = 4, indent_head = TRUE)
  output_context
}

#' custom_context
#'
#' @description the _context fuctions define lists of parameters for text formatting.
#' The specific contexts return hard-coded values appropriate to the context they describe, while custom_context allows definition of new contexts for custom formatting.
#' @param output_mode A named output mode, controls the target of the _paste functions options are "rstudioapi" or "console"
#' @param nspc The number of spaces for each indent level in the output context
#' @param indent_context The number of spaces applied initially to all lines in the output context
#' @param indent_head Logical. Apply the indent_context to the to the header row? Use FALSE if targetting cursor location.
#' @return an output context. An input to _paste, _format, _contruct functions used to formatt whitespace.
#' @export
#'
custom_context <- function(output_mode = "console", nspc = 2, indent_context = 0, indent_head = TRUE){
  output_context <- list(output_mode = output_mode, nspc = nspc, indent_context = indent_context, indent_head = indent_head)
  output_context
}

#' Title
#'
#' @param char_vec a character string with characters that may need escaping (eg. `"`)
#'
#' @return the original char_vec with an extra "\" inserted before each character that needs escaping.
escape_chars <- function(char_vec){
  gsub(pattern = "(\"|\')", replacement = "\\\\\\1", x = char_vec, fixed = FALSE)
}

