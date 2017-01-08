globalVariables(".rs.readUiPref", "datapasta") #ignore this function in R CMD checks, since it is part of RStudio runtime

#' tribble_paste
#' @description Parse the current clipboard as a table and paste in at the cursor location in tribbble format.
#' @return The parsed table text. Useful for testing.
#' @export
#'
tribble_paste <- function(){
  clipboard_table <- tryCatch({read_clip_tbl_guess()},
                               error = function(e) {
                                 return(NULL)
                               })

  if(is.null(clipboard_table)){
    if(!clipr::clipr_available()) message("Clipboard is not available. Is R running in RStudio Server or a C.I. machine?")
    else message("Could not paste clipboard as tibble. Text could not be parsed as table.")
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

  #Find the max length of data as string in each column
  col_widths <- vapply(X = clipboard_table,
                       FUN.VALUE = numeric(1),
                       FUN =
                         function(col){nchar
                           max( vapply(X = col,
                                       FUN = nchar,
                                       FUN.VALUE = numeric(1)
                                ),
                           na.rm = TRUE
                           )
                         }

  )
  #Set the column width depending on the max length of data as string or the header, whichever is longer.
  col_widths <- mapply(max,
                       col_widths+2, #+2 for quotes ""
                       nchar(names(clipboard_table))+1) #+1 for "~"

  #Header
  header <- "tribble(\n"

  #Column names
  names_row <- paste0(
                  paste0(strrep(" ",indent_context+nspc),
                      paste0(
                        paste0(
                          mapply(
                            pad_to,
                            paste0("~",names(clipboard_table)),
                            col_widths
                          ),
                          ","
                        ),
                        collapse = " "
                      )
                    ), "\n"
                )

  #Parse data types from string using readr::parse_guess
  clipboard_table_types <- lapply(clipboard_table, readr::guess_parser)


  #Write correct data types
  body_rows <- lapply(X = as.data.frame(t(clipboard_table), stringsAsFactors = FALSE),
                      FUN =
                        function(col){
                          paste0(strrep(" ",indent_context+nspc),
                            paste0(
                                   paste0(
                                     mapply(
                                       render_type_pad_to,
                                       col,
                                       clipboard_table_types,
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
  footer <- paste0(strrep(" ",indent_context+nspc),")")
  output <- paste0(header, names_row, body_rows, footer)
  rstudioapi::insertText(output)
  output
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
    if(is.na(char_vec)){
        output <- switch(char_type,
                         "integer" = "NaN",
                         "double" = "NaN",
                         "logical" = "NA",
                         "character" = "NA",
                         "NA"
                  )
    }else{
        output <- switch(char_type,
                         "integer" = as.integer(char_vec),
                         "double" = as.double(char_vec),
                         "logical" = as.logical(char_vec),
                         "character" = ifelse(nchar(char_vec)!=0, paste0('"',char_vec,'"'), "NA"),
                         paste0('"',char_vec,'"')
                  )

    }
    pad_to(output, char_length)
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
  do.call(utils::read.table, args = .dots)
}


