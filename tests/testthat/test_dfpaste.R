context("test data.frame paste")
skip_msg <- "System clipboard is not available - skipping test."
is_clipr_available <- clipr::clipr_available()
is_rstudio_available <- rstudioapi::isAvailable()

test_that("Test text wrapping works", {
  skip_if_not(is_clipr_available, skip_msg)
  skip_if_not(is_rstudio_available)
  skip_on_cran()
  expect_equal({
    clipr::write_clip(content = readr::read_lines(file = "./route_length.txt"))
    df_paste()
  },
  {"data.frame(\n                Route = c(40010, 40015, 40020, 40025, 40030, 40035, 40040,\n                          40045, 40050, 40055, 40060, 40065, 40070, 40075,\n                          40080, 40085, 40090, 40092, 40095, 40100, 40105, 40110,\n                          40115, 40120, 40125, 40130, 40135, 40140, 40145, 40150,\n                          41010, 41015, 41020, 41025, 41030, 41035, 41040, 41045,\n                          41050, 41055, 41060, 41065, 41070, 41075, 41080, 41085,\n                          41090, 41095, 41100, 41105, 41110, 41115, 41120, 41125,\n                          41130, 41135, 41140, 41145, 41150, 41155, 41160),\n   Approximate.length = c(2000, 500, 2000, 500, 500, 500, 1500, 2000, 1500,\n                          500, 2000, 500, 4000, 500, 1000, 500, 1000, 2000,\n                          2500, 500, 1000, 500, 3500, 3500, 500, 6000, 500, 3000,\n                          1000, 2000, 2000, 1000, 3000, 500, 5500, 1000, 3500,\n                          3500, 500, 1000, 500, 1000, 500, 2000, 1500, 500, 3500,\n                          500, 500, 1500, 1000, 500, 1000, 2000, 1000, 1000, 500,\n                          500, 2000, 500, 2000)\n)"
  })
})

test_that("A pasted Data.frame can be parsed correctly", {
  skip_if_not(is_clipr_available, skip_msg)
  skip_if_not(is_rstudio_available)
  skip_on_cran()
  expect_equal({
    clipr::write_clip(content = readr::read_lines(file = "./route_length.txt"))
    eval(parse(text = df_paste()))
  },
  {
    data.frame(
                    Route = c(40010, 40015, 40020, 40025, 40030, 40035, 40040,
                              40045, 40050, 40055, 40060, 40065, 40070, 40075,
                              40080, 40085, 40090, 40092, 40095, 40100, 40105,
                              40110, 40115, 40120, 40125, 40130, 40135, 40140,
                              40145, 40150, 41010, 41015, 41020, 41025, 41030, 41035,
                              41040, 41045, 41050, 41055, 41060, 41065, 41070,
                              41075, 41080, 41085, 41090, 41095, 41100, 41105,
                              41110, 41115, 41120, 41125, 41130, 41135, 41140,
                              41145, 41150, 41155, 41160),
       Approximate.length = c(2000, 500, 2000, 500, 500, 500, 1500, 2000, 1500,
                              500, 2000, 500, 4000, 500, 1000, 500, 1000, 2000,
                              2500, 500, 1000, 500, 3500, 3500, 500, 6000, 500,
                              3000, 1000, 2000, 2000, 1000, 3000, 500, 5500,
                              1000, 3500, 3500, 500, 1000, 500, 1000, 500, 2000,
                              1500, 500, 3500, 500, 500, 1500, 1000, 500, 1000, 2000,
                              1000, 1000, 500, 500, 2000, 500, 2000)
    )
  })
})

test_that("A pasted multi-type data.frame is rendered and parsed correctly", {
  skip_if_not(is_clipr_available, skip_msg)
  skip_on_cran()
  skip_if_not(is_rstudio_available)
  expect_equal({
    clipr::write_clip(content = readr::read_lines(file = "./brisbane_weather.txt"))
    eval(parse(text = df_paste()))
  },
  {
    data.frame(
      Precis.Icon = c("Possible shower.", "Mostly sunny.",
                      "Shower or two. Possible storm.", "Possible shower.",
                      "Shower or two. Possible storm.", "Possible shower or storm.",
                      "Possible shower or storm.", "Mostly sunny.", "Possible shower.",
                      "Possible shower.", "Mostly sunny."),
      Location = c("Brisbane", "Brisbane Airport", "Beaudesert", "Chermside",
                   "Gatton", "Ipswich", "Logan Central", "Manly",
                   "Mount Gravatt", "Oxley", "Redcliffe"),
      Min = c(17, 16, 14, 16, 14, 14, 16, 18, 16, 15, 18),
      Max = c(29, 28, 30, 30, 32, 31, 30, 26, 29, 30, 28)
    )
  })
})



