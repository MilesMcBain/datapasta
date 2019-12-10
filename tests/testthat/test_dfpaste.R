context("test data.frame paste")
skip_msg <- "System clipboard is not available - skipping test."
is_clipr_available <- clipr::clipr_available()
is_RStudio_session <- interactive() & rstudioapi::isAvailable()

test_that("Test data.frame text wrapping works", {
  skip_if_not(is_clipr_available, skip_msg)
  skip_on_cran()
  skip_if_not(is_RStudio_session)
  expect_equal({
    clipr::write_clip(content = readr::read_lines(file = "./route_length.txt"))
    dfdt_construct(class = "data.frame")
  },
  {
    "data.frame(\n               Route = c(40010L,40015L,40020L,\n                         40025L,40030L,40035L,40040L,40045L,40050L,40055L,\n                         40060L,40065L,40070L,40075L,40080L,40085L,40090L,\n                         40092L,40095L,40100L,40105L,40110L,40115L,40120L,\n                         40125L,40130L,40135L,40140L,40145L,40150L,41010L,\n                         41015L,41020L,41025L,41030L,41035L,41040L,41045L,\n                         41050L,41055L,41060L,41065L,41070L,41075L,\n                         41080L,41085L,41090L,41095L,41100L,41105L,41110L,\n                         41115L,41120L,41125L,41130L,41135L,41140L,41145L,\n                         41150L,41155L,41160L),\n  Approximate.length = c(2000L,500L,2000L,500L,\n                         500L,500L,1500L,2000L,1500L,500L,2000L,500L,\n                         4000L,500L,1000L,500L,1000L,2000L,2500L,500L,1000L,\n                         500L,3500L,3500L,500L,6000L,500L,3000L,1000L,\n                         2000L,2000L,1000L,3000L,500L,5500L,1000L,3500L,\n                         3500L,500L,1000L,500L,1000L,500L,2000L,1500L,\n                         500L,3500L,500L,500L,1500L,1000L,500L,1000L,2000L,\n                         1000L,1000L,500L,500L,2000L,500L,2000L)\n)"
  })
})

test_that("A Data.frame can be parsed correctly", {
  skip_if_not(is_clipr_available, skip_msg)
  skip_on_cran()
  skip_if_not(is_RStudio_session)
  expect_equal({
    clipr::write_clip(content = readr::read_lines(file = "./route_length.txt"))
    eval(parse(text = dfdt_construct(class = "data.frame")))
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
  skip_if_not(is_RStudio_session)
  expect_equal({
    clipr::write_clip(content = readr::read_lines(file = "./brisbane_weather.txt"))
    eval(parse(text = dfdt_construct(class = "data.frame")))
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
  skip_if_not(is_RStudio_session)
  expect_equal({
    clipr::write_clip(content = c("char,int", "a,1", "b,3"))
    eval(parse(text = dfdt_construct(class = "data.frame")))
  },
  {
    data.frame(stringsAsFactors=FALSE,
               char = c("a", "b"),
               int = c(1, 3)
    )
  })
})

test_that("Data frame contruct recognises raw data with no column headings and adds dummy headers", {
  skip_if_not(is_clipr_available, skip_msg)
  skip_on_cran()
  skip_if_not(is_RStudio_session)
  expect_equal(
  { data.frame(stringsAsFactors=FALSE,
          V1 = c(52.4, 53.4, 86, 73, 79, 73),
          V2 = c(46.9, 52, 86.6, 73.3, 79.5, 73.5),
          V3 = c(33.7, 51.8, 84, 71, 77.5, 73.6),
          V4 = c("A", "A", "B", "B", "B", "C"))
  },
  { clipr::write_clip(readr::read_lines("./just_data.txt"))
    eval(parse(text = dfdt_construct(class = "data.frame")))}
  )
})


test_that("Columns with non-valid names can be parsed as a data.frame, with names surrounded by backticks and check.names FALSE", {
  skip_if_not(is_clipr_available, skip_msg)
  skip_on_cran()
  skip_if_not(is_RStudio_session)
  expect_equal(
    {clipr::write_clip(readr::read_lines(file = "./non_valid_colnames.txt"))
      eval(parse(text = dfdt_construct(class = "data.frame")))},
    {
      data.frame(stringsAsFactors=FALSE,
                 check.names=FALSE,
                 `!!` = c(1L),
                 `2015` = c("b"),
                 `%` = c(3L),
                 `TRUE` = c("D")
      )
    }
  )
})

test_that("meaningful rownames are included when input_table is a data.frame", {
  expect_equal(
    eval(parse(text = dfdt_construct(mtcars[1:3, 1:3], class = "data.frame"))),
    data.frame(stringsAsFactors=FALSE,
      row.names = c("Mazda RX4", "Mazda RX4 Wag", "Datsun 710"),
      mpg = c(21, 21, 22.8),
      cyl = c(6, 6, 4),
      disp = c(160, 160, 108)
    )
  )
  expect_equal(
    eval(parse(text = dfdt_construct(iris[1:3, 1:3], class = "data.frame"))),
    data.frame(
      Sepal.Length = c(5.1, 4.9, 4.7),
      Sepal.Width = c(3.5, 3, 3.2),
      Petal.Length = c(1.4, 1.4, 1.3)
    )
  )
  expect_equal(
    eval(parse(text = dfdt_construct(`rownames<-`(data.table::as.data.table(mtcars[1:3, 1:3]), rownames(mtcars[1:3, 1:3])), class = "data.table"))),
    data.table::data.table(stringsAsFactors=FALSE,
      mpg = c(21, 21, 22.8),
      cyl = c(6, 6, 4),
      disp = c(160, 160, 108)
    )
  )
  expect_equal(
    eval(parse(text = dfdt_construct(data.table::as.data.table(mtcars[1:3, 1:3]), class = "data.table"))),
    data.table::data.table(stringsAsFactors=FALSE,
      mpg = c(21, 21, 22.8),
      cyl = c(6, 6, 4),
      disp = c(160, 160, 108)
    )
  )
})

test_that("data.frame output is aligned when multiple cols and args are used", {
  expect_equal(
    dfdt_construct(cbind(mtcars[1:3, 1:3],
                         letters[1:3],
                         stringsAsFactors = FALSE),
                   class = "data.frame"),
'data.frame(
  stringsAsFactors = FALSE,
       check.names = FALSE,
         row.names = c("Mazda RX4", "Mazda RX4 Wag", "Datsun 710"),
               mpg = c(21, 21, 22.8),
               cyl = c(6, 6, 4),
              disp = c(160, 160, 108),
    `letters[1:3]` = c("a", "b", "c")
)')

  expect_equal(
    dfdt_construct(data.frame(a_really_reall_really_long_name = c(1000, 2000, 3000),
                              letters = letters[1:3],
                              row.names = month.name[1:3],
                              stringsAsFactors = FALSE),
                   class = "data.frame"),
'data.frame(
                 stringsAsFactors = FALSE,
                        row.names = c("January", "February", "March"),
  a_really_reall_really_long_name = c(1000, 2000, 3000),
                          letters = c("a", "b", "c")
)')

})

test_that("1 col data.frames paste without repetition", {
  expect_equal(eval(parse(text = dfdt_construct(data.frame(x = c(1,2,3)),
                                 class = "data.frame"))),
               data.frame(x = c(1,2,3)))
  })

test_that("Column names are not interferred with", {
  expect_equal(eval(parse(text = dfdt_construct(data.frame(`99` = c(1,2,3),
                                                           check.names = FALSE),
                                 class = "data.frame"))),
               data.frame(`99` = c(1,2,3),
                          check.names = FALSE))})
