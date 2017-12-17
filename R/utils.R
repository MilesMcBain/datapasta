first_row_to_header <- function(a_table){
  colnames(a_table) <- a_table[1, ]
  colnames(a_table) <- gsub(" ", ".", colnames(a_table))
  a_table <- a_table[-1, ]
  rownames(a_table) <- seq(nrow(a_table))
  a_table
}
