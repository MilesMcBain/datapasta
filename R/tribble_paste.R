#' tribble_paste
#' @description Parse the current clipboard as a table and paste in at the cursor location in tribbble format.
#' @return nothing.
#' @export
#'
tribble_paste <- function(){
  clipboard_table <- clipr::read_clip_tbl()
  nspc <- .rs.readUiPref('num_spaces_for_tab')
  context <- rstudioapi::getActiveDocumentContext()
  context_row <- context$selection[[1]]$range$end["row"]
  indent_context <- nchar(context$contents[context_row])


  #Find the max length of data in each column
  col_widths <- vapply(X = clipboard_table,
                       FUN.VALUE = numeric(1),
                       FUN =
                         function(col){
                           max( vapply(X = col,
                                       FUN = nchar,
                                       FUN.VALUE = numeric(1)
                                )
                           )
                         }

  )
  #Set the column width depending on the max length or the header, whichever is longer.
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


  body_rows <- lapply(X = as.data.frame(t(clipboard_table), stringsAsFactors = FALSE),
                      FUN =
                        function(col){
                          paste0(strrep(" ",indent_context+nspc),
                            paste0(
                                   paste0(
                                     mapply(
                                       pad_to,
                                       paste0('"',col,'"'),
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

  rstudioapi::insertText(paste0(header, names_row, body_rows, footer))
}


#' pad_to
#' @description Left pad string to a certain size. A helper function for getting spacing in table correct.
#' @param char_vec character vector.
#' @param char_length length to pad to.
#'
#' @return
#'
pad_to <-function(char_vec, char_length){
  paste0(strrep(" ",char_length - nchar(char_vec)),char_vec)
}




