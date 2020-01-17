first_row_to_header <- function(a_table){
  colnames(a_table) <- a_table[1, , drop = FALSE]
  colnames(a_table) <- gsub(" ", ".", colnames(a_table))
  a_table <- a_table[-1, , drop = FALSE]
  rownames(a_table) <- seq(nrow(a_table))
  a_table
}

is_rstudio_selection <- function(){
  context <- rstudioapi::getActiveDocumentContext()
  !all(context$selection[[1]]$range$start ==
         context$selection[[1]]$range$end)
}

is_tibble <-  function(x) inherits(x, "tbl_df")

is_data.table <- function(x) inherits(x, "data.table")
