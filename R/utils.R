first_row_to_header <- function(a_table){
  colnames(a_table) <- a_table[1, ]
  colnames(a_table) <- gsub(" ", ".", colnames(a_table))
  a_table <- a_table[-1, ]
  rownames(a_table) <- seq(nrow(a_table))
  a_table
}

is_rstudio_selection <- function(){
  context <- rstudioapi::getActiveDocumentContext()
  !all(context$selection[[1]]$range$start ==
         context$selection[[1]]$range$end)
}


