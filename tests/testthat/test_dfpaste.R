context("test data.frame paste")
skip_msg <- "System clipboard is not available - skipping test."
is_clipr_available <- clipr::clipr_available()
is_rstudio_available <- rstudioapi::isAvailable()

test_that("Test data.frame text wrapping works", {
  skip_if_not(is_clipr_available, skip_msg)
  skip_if_not(is_rstudio_available)
  skip_on_cran()
  expect_equal({
    clipr::write_clip(content = readr::read_lines(file = "./route_length.txt"))
    df_construct()
  },
  {"data.frame(\n                Route = c(40010L, 40015L, 40020L, 40025L, 40030L, 40035L,\n                          40040L, 40045L, 40050L, 40055L, 40060L, 40065L,\n                          40070L, 40075L, 40080L, 40085L, 40090L, 40092L, 40095L,\n                          40100L, 40105L, 40110L, 40115L, 40120L, 40125L, 40130L,\n                          40135L, 40140L, 40145L, 40150L, 41010L, 41015L, 41020L,\n                          41025L, 41030L, 41035L, 41040L, 41045L, 41050L, 41055L,\n                          41060L, 41065L, 41070L, 41075L, 41080L, 41085L, 41090L,\n                          41095L, 41100L, 41105L, 41110L, 41115L, 41120L, 41125L,\n                          41130L, 41135L, 41140L, 41145L, 41150L, 41155L,\n                          41160L),\n   Approximate.length = c(2000L, 500L, 2000L, 500L, 500L, 500L, 1500L, 2000L,\n                          1500L, 500L, 2000L, 500L, 4000L, 500L, 1000L, 500L,\n                          1000L, 2000L, 2500L, 500L, 1000L, 500L, 3500L, 3500L,\n                          500L, 6000L, 500L, 3000L, 1000L, 2000L, 2000L, 1000L,\n                          3000L, 500L, 5500L, 1000L, 3500L, 3500L, 500L, 1000L,\n                          500L, 1000L, 500L, 2000L, 1500L, 500L, 3500L, 500L, 500L,\n                          1500L, 1000L, 500L, 1000L, 2000L, 1000L, 1000L, 500L,\n                          500L, 2000L, 500L, 2000L)\n)"
  })
})

test_that("A Data.frame can be parsed correctly", {
  skip_if_not(is_clipr_available, skip_msg)
  skip_if_not(is_rstudio_available)
  skip_on_cran()
  expect_equal({
    clipr::write_clip(content = readr::read_lines(file = "./route_length.txt"))
    eval(parse(text = df_construct()))
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
    eval(parse(text = df_construct()))
  },
  {
    data.frame(stringsAsFactors=FALSE,
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

test_that("stringsAsFactors=FALSE is added correctly", {
  skip_if_not(is_clipr_available, skip_msg)
  skip_on_cran()
  skip_if_not(is_rstudio_available)
  expect_equal({
    clipr::write_clip(content = c("char,int", "a,1", "b,3"))
    eval(parse(text = df_construct()))
  },
  {
    data.frame(stringsAsFactors=FALSE,
               char = c("a", "b"),
               int = c(1, 3)
    )
  })
})




