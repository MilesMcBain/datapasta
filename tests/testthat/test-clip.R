context("clipboard alternatives")

test_that("read_file_to_clipr_vec", {
  skip_if_not(clipr::clipr_available())
  skip_on_cran()

  # Example text
  text <- readr::read_lines(file = "./brisbane_weather.txt")

  # Round trip through clipboard
  clipr::write_clip(text)
  text_clipped <- clipr::read_clip()

  # as read from file: ask_user_for_paste()
  text_scanned_file <- read_file_to_clipr_vec('./brisbane_weather.txt')

  # as read through connection: read_rstudio_editor()
  text_flat <- paste(text, collapse = "\n")
  txtcon <- textConnection(text_flat)
  text_scanned_con <- read_file_to_clipr_vec(txtcon)

  expect_equal(text_scanned_file, text_clipped)
  expect_equal(text_scanned_con, text_clipped)
})
