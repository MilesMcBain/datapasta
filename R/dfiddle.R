#' dfiddle
#' @description An addin to fiddle your RStudio selections to better things.
#' Make a selection in RStudio and dfiddle will update it inline. Good for:
#' Converting Text to vectors (`c()`), pivoting between horizontal and vertical vectors,
#' reflowing tribble() and data.frame() definitions to have nice indenting and padding.
#'
#' @return a fiddled version of your selection (invisibly)
#' @export
zzz_rs_dfiddle <- function(){
  if(!is_rstudio_selection()) return(invisible(NULL))

  # If we have a data.frame or tibble, repaste (or replace) to get a neatly formatted output.

  doc_context <- rstudioapi::primary_selection(rstudioapi::getActiveDocumentContext())
  indent_context <- attr(regexpr(pattern = "^\\s*(?=\\S)", text = doc_context$text, perl = TRUE), "match.length")
  eval_env <- new.env()

  selection_result <-
    tryCatch( expr = eval(parse(text = doc_context$text), envir = eval_env),
              error = function(e) NULL)

  if(is_tibble(selection_result) |
     is.data.frame(selection_result)){
    if(is_tibble(selection_result)) {
      table_form <- tribble_construct(selection_result, oc = rstudio_context())
    } else if (is_data.table(selection_result)) {
      table_form <- dfdt_construct(selection_result, oc = rstudio_context(), class = "data.table")
    } else {
      table_form <- dfdt_construct(selection_result, oc = rstudio_context(), class = "data.frame")
    }
    nlines <- n_lines(table_form)
    end_of_range <- last_line_content_length(table_form) + indent_context + 1
    insert_range <- rstudioapi::document_range(doc_context$range$start,
                                               c(doc_context$range$start[1] + n_lines(table_form)-1, 1)
                    )
    rstudioapi::modifyRange(doc_context$range, text = table_form)
    rstudioapi::setSelectionRanges(insert_range)
    return(invisible(table_form)) # done
  }

  # It wasn't a tibble or data.frame, let's try vector options

  if(is_naked_vec(doc_context$text)){
    regular_delimited <-
      paste0(split_naked_vec(doc_context$text), collapse=", ")
    vector_form <- paste0("c(",regular_delimited,")")
    insert_range <- rstudioapi::document_range(doc_context$range$start,
                                               c(doc_context$range$start[1], nchar(vector_form)+1)
                                               )
  } # parse to a vector

  if(is_horiz_vec(doc_context$text)){
    vector_form <-
      paste0(strrep(" ", indent_context), "c(",
        paste0(
          split_horiz_vec(doc_context$text),
          collapse = paste0(",\n",strrep(" ", indent_context + (doc_context$range$start[2]-1) + 2)) # 2 to align for 'c('
        ),
        ")"
      )
    end_of_range <- last_line_content_length(vector_form) + indent_context + 1
    insert_range <-
      rstudioapi::document_range(doc_context$range$start,
                     c(doc_context$range$start[1] + n_lines(vector_form)-1, end_of_range)
      )
   } # pivot to vertical vector

  if(is_vert_vec(doc_context$text)){
    regular_delimited <- paste0(split_vert_vec(doc_context$text), collapse = ",")
    vector_form <- paste0(strrep(" ", indent_context),"c(",regular_delimited,")")
    insert_range <- rstudioapi::document_range(doc_context$range$start,
                                               c(doc_context$range$start[1], nchar(vector_form)+1)
    )

  } # pivot horiz vector

  rstudioapi::modifyRange(doc_context$range, text = vector_form)
  rstudioapi::setSelectionRanges(insert_range)
  return(invisible(vector_form))

}

#' Toggle Quotes
#' @description An addin to toggle between quotes and bare vectors. Applies to a vector selected
#' in an RStudio source editor. Works with horizontal or vertical form.
#' @return The toggled vector (invisibly).
#' @export
zzz_rs_toggle_quotes <- function(){
  if(!is_rstudio_selection()) return(invisible(NULL))
  doc_context <- rstudioapi::primary_selection(rstudioapi::getActiveDocumentContext())
  indent_context <- attr(regexpr(pattern = "^\\s*(?=\\S)", text = doc_context$text, perl = TRUE), "match.length")
  vector_form <- NA

  if(is_vert_vec(doc_context$text)){
    content <- toggle_quote_elems( split_vert_vec(doc_context$text) )

    vector_form <-
      paste0(strrep(" ", indent_context), "c(",
             paste0(
               content,
               collapse = paste0(",\n",strrep(" ", indent_context + (doc_context$range$start[2]-1) + 2)) # 2 to align for 'c('
             ),
             ")"
      )
    end_of_range <- last_line_content_length(vector_form) + indent_context + 1
    insert_range <-
      rstudioapi::document_range(doc_context$range$start,
                                 c(doc_context$range$start[1] + n_lines(vector_form)-1, end_of_range)
      )

  }

  if(is_horiz_vec(doc_context$text)){
    content <- toggle_quote_elems( split_horiz_vec(doc_context$text) )

    vector_form <- paste0(strrep(" ", indent_context),"c(",
                          paste0(content , collapse = ","),
                          ")")
    insert_range <- rstudioapi::document_range(doc_context$range$start,
                                               c(doc_context$range$start[1], nchar(vector_form)+1)
    )

  }
  if(!is.na(vector_form)){
    rstudioapi::modifyRange(doc_context$range, text = vector_form)
    rstudioapi::setSelectionRanges(insert_range)
    return(invisible(vector_form))
  }

}

#########################################################################
# Utilities for dfiddle addins
########################################################################

is_naked_vec <- function(text){
 text <- trimws(text)
 naked_vector <- "^(((\"|\')[^\'\"]+(\"|\')|\\w+)([[:blank:]])*(,)*(\n)*([[:blank:]])*)+"
 matches <- grep(x = text, pattern = naked_vector)
 length(matches) > 0 && matches == 1
}

split_naked_vec<- function(naked_vec){
  naked_vec <- trimws(naked_vec)
  trimws(strsplit(x = naked_vec, split = "(?<=(\"|\'|\\w))[[:blank:],\n]+",
           perl = TRUE)[[1]])
}

is_horiz_vec <- function(text){
  text <- trimws(text)
  horiz_vector <- "c\\(([[:blank:]]*((\"|\')[^\'\"]+(\"|\')|\\w+)[[:blank:]]*,)+[[:blank:]]*((\"|\')[^\'\"]+(\"|\')|\\w+)[[:blank:]]*\\)"
  matches <- grep(x = text, pattern = horiz_vector)
  length(matches) > 0 && matches == 1
}

split_horiz_vec <- function(horiz_vec){
  horiz_vec <- trimws(horiz_vec)
  horiz_elems <- gsub( pattern = "(^c\\(|\\)$)",
                       replacement = "",
                       x = horiz_vec)
  trimws(strsplit(x = horiz_elems, split = "(?<=(\"|\'|\\w))[[:blank:]]*,",
    perl = TRUE)[[1]])
}

is_vert_vec <- function(text){
  text <- trimws(text)
  vert_vector <- "c\\(((\\s)*((\"|\')[^\'\"]+(\"|\')|\\w+)[[:blank:]]*,[[:blank:]]*\n)+[[:blank:]]*((\"|\')[^\'\"]+(\"|\')|\\w+)[[:blank:]]*\\)"
  matches <- grep(x = text, pattern = vert_vector)
  length(matches) > 0 && matches == 1
}

split_vert_vec <- function(vert_vec){
  vert_vec <- trimws(vert_vec)
  vert_elems <- gsub( pattern = "(^c\\(|\\)$)",
                      replacement = "",
                      x = vert_vec)
  trimws(strsplit(vert_elems, split = "(?<=(\"|\'|\\w))[[:blank:]]*,[[:blank:]]*\n", perl = TRUE)[[1]])
}

n_lines <- function(a_structure){
  matches <- gregexpr(pattern = "\n", text = a_structure)[[1]]
  if(length(matches) == 1 && matches == -1) 1
  else length(matches) + 1 # The last line won't have a /n on it so +1.
}

last_line_content_length <- function(a_structure){
  if(length(grep(pattern = "\n$", a_structure)) != 0) 0
  else attr(gregexpr(pattern = "(?<=\n).*$", text = a_structure, perl = TRUE)[[1]], "match.length")
}

toggle_quote_elems <- function(content){
  quoted_content <- vapply(content, is_elem_quoted, logical(1))
  # If not all quoted, then quote all
  # If all quotes unqoute all
  if( !(all(quoted_content)) ){
    content[!quoted_content] <- vapply(content[!quoted_content], quote_elem, character(1), USE.NAMES = FALSE)
  } else {
    content <- vapply(content, unquote_elem, character(1), USE.NAMES = FALSE)
  }
  content
}

is_elem_quoted <- function(chr_vec){
  if(length(chr_vec) != 1) stop("is_elem_quoted called on char vec of length > 1")
  matches <- grep(pattern = "^[\"\'].*[\"\']$", x = chr_vec, perl = TRUE)
  length(matches) > 0 && matches == 1
}

quote_elem <- function(chr_vec){
  if(length(chr_vec) != 1) stop("quote_elem called on char vec of length > 1")
  paste0('"',chr_vec,'"')
}

unquote_elem <- function(chr_vec){
  if(length(chr_vec) != 1) stop("unquote_elem called on char vec of length > 1")
  gsub(pattern ="(^[\'\"])|([\'|\"]$)", replacement = "", x = chr_vec, perl = TRUE)
}




















