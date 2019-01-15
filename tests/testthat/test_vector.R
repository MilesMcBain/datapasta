context("test vector paste")
skip_msg <- "System clipboard is not available - skipping test."
is_clipr_available <- clipr::clipr_available()
is_RStudio_session <- interactive() & rstudioapi::isAvailable()


test_that("A vector of strings is formatted correctly", {
  skip_if_not(is_clipr_available, skip_msg)
  skip_if_not(is_RStudio_session)
  skip_on_cran()
  expect_equal({
    clipr::write_clip(content = c("Normal",
                                  "Steel",
                                  "Psychic",
                                  "Dark",
                                  "Dragon",
                                  "Normal",
                                  "Water",
                                  "Flying"))
    vector_construct()
  },
  {
    "c(\"Normal\", \"Steel\", \"Psychic\", \"Dark\", \"Dragon\", \"Normal\", \"Water\", \"Flying\")\n"
  }


  )
})

test_that("vector_construct handles numeric correctly ", {
  skip_if_not(is_clipr_available, skip_msg)
  skip_if_not(is_RStudio_session)
  skip_on_cran()
  expect_equal({
    suppressWarnings(clipr::write_clip(content = c(7,
                                                   8,
                                                   7,
                                                   6,
                                                   6,
                                                   NA,
                                                   6,
                                                   9))
    )
    vector_construct()
  },
  "c(7L, 8L, 7L, 6L, 6L, NA, 6L, 9L)\n"
  )
})



test_that("vector_construct combines numeric/strings correctly ", {
    skip_if_not(is_clipr_available, skip_msg)
    skip_if_not(is_RStudio_session)
    skip_on_cran()
    expect_equal({
        suppressWarnings(clipr::write_clip(content = c("a","6", "b", "4"))
        )
        vector_construct()
    },
    "c(\"a\", \"6\", \"b\", \"4\")\n"
    )
})


test_that("vector_construct handles empty strings", {
    skip_if_not(is_clipr_available, skip_msg)
    skip_if_not(is_RStudio_session)
    skip_on_cran()
    expect_equal({
    suppressWarnings(clipr::write_clip(content = c("a","6", "", "4"))
    )
    vector_construct()
  },
  "c(\"a\", \"6\", NA, \"4\")\n"
  )
})

test_that("vector_construct strips whitespace from natural integer lists on a single line",{
  skip_if_not(is_clipr_available, skip_msg)
  skip_if_not(is_RStudio_session)
  skip_on_cran()
  expect_equal({
    suppressWarnings(clipr::write_clip(content = "1, 2, 3, 4"))
    eval(parse(text = vector_construct()))
  },{
    c(1L, 2L, 3L, 4L)
  })
})

test_that("vector_construct strips whitespace and quotes from natural string lists on a single line",{
  skip_if_not(is_clipr_available, skip_msg)
  skip_if_not(is_RStudio_session)
  skip_on_cran()
  expect_equal({
    suppressWarnings(clipr::write_clip(content = "\"a\", \"b\", \"c\""))
    eval(parse(text = vector_construct()))
  },{
    c("a", "b", "c")
  })
})

test_that("vector_construct strips whitespace and quotes from natural string lists over multiple lines",{
  skip_if_not(is_clipr_available, skip_msg)
  skip_if_not(is_RStudio_session)
  skip_on_cran()
  expect_equal({
    suppressWarnings(clipr::write_clip(content = c("      \"hms\",", "      \"jsonlite\",", "      \"lubridate\",",
                                                   "      \"magrittr\",", "      \"modelr\",")))
    eval(parse(text = vector_construct()))
  },{
    c("hms", "jsonlite", "lubridate", "magrittr", "modelr")
  })
})

test_that("vector_construct handles leading and lagging whitespace on unquoted char lists", {
  skip_if_not(is_clipr_available, skip_msg)
  skip_if_not(is_RStudio_session)
  skip_on_cran()
  expect_equal({
    suppressWarnings(clipr::write_clip(content = "Mint, Fedora, Debian, Ubuntu, OpenSUSE")
    )
    vector_construct()
  },
  "c(\"Mint\", \"Fedora\", \"Debian\", \"Ubuntu\", \"OpenSUSE\")\n"
  )
})

test_that("vector_construct splits appart and parses input character arguments of length 1",{
  expect_equal({eval(parse(text = vector_construct("1,2,3,4")))},
               {c(1L, 2L, 3L, 4L)})
})

test_that("vector_construct escapes backslashes correctly",{
  expect_equal({eval(parse(text = vector_construct(
    readr::read_lines("./backslashes.txt")
  )))},
  {c("\\\\my-server\\DATA\\libraries")})
})

test_that("vectors of length 1 are handled correctly",{

  expect_equal({
    eval(parse(text =
                 vector_construct("Mint	Fedora	Debian	Ubuntu	OpenSUSE")))
    },
    {
      c("Mint", "Fedora", "Debian", "Ubuntu", "OpenSUSE")
    })

  expect_equal({
    eval(parse(text =
                 vector_construct("Mint, Fedora, Debian, Ubuntu, OpenSUSE")))
    },
    {
      c("Mint", "Fedora", "Debian", "Ubuntu", "OpenSUSE")
    })

  expect_equal({
    eval(parse(text =
                 vector_construct("Mint Fedora Debian Ubuntu OpenSUSE")))
    },
    {
      c("Mint", "Fedora", "Debian", "Ubuntu", "OpenSUSE")
    })

  expect_equal({
    eval(parse(text =
                 vector_construct("  Mint  Fedora  Debian  Ubuntu  OpenSUSE  ")))
    },
    {
      c("Mint", "Fedora", "Debian", "Ubuntu", "OpenSUSE")
    })
})

test_that("readr integer guessing is still possible",
          {
            ## This ability was changed in readr 1.2.0
            ## much of datapasta depends on this but since many clipboard tests
            ## are skipped on CRAN it was not picked up until later.
            ## This tests the behaviour persists so I will get a notification if
            ## it is changed further.
            expect_equal(
              {readr::guess_parser(c("1", "2", "3"), guess_integer = TRUE)},
              {"integer"}
          )})
