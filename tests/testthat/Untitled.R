c("b", "hi", "there", "", "robot")

x <- clipr::read_clip()

text <- textConnection(x)

a_table <- read.table(text, sep = '\t')

is.na(a_table)

View(a_table)


b_table <- read.table(textConnection(x), sep='\t', stringsAsFactors = FALSE, header = TRUE)

guess_sep(paste0(x, collapse = "\n"))


tribble(
     ~a,      ~b,
      2,    "hi",
      3, "there",
      4, "robot"
    )

