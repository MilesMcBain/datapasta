context("test tribble paste")
skip_msg <- "System clipboard is not available - skipping test."
is_clipr_available <- clipr::clipr_available()

test_that("Brisbane Weather is parsed", {

    expect_equal(
        read_clip_tbl_guess(readr::read_lines(file = "./brisbane_weather.txt")),
        as.data.frame(tibble::tribble(
                            ~Precis.Icon,          ~Location, ~Min, ~Max,
                      "Possible shower.",         "Brisbane", "17", "29",
                         "Mostly sunny.", "Brisbane Airport", "16", "28",
        "Shower or two. Possible storm.",       "Beaudesert", "14", "30",
                      "Possible shower.",        "Chermside", "16", "30",
        "Shower or two. Possible storm.",           "Gatton", "14", "32",
             "Possible shower or storm.",          "Ipswich", "14", "31",
             "Possible shower or storm.",    "Logan Central", "16", "30",
                         "Mostly sunny.",            "Manly", "18", "26",
                      "Possible shower.",    "Mount Gravatt", "16", "29",
                      "Possible shower.",            "Oxley", "15", "30",
                         "Mostly sunny.",        "Redcliffe", "18", "28"
        )
    ))
})

test_that("Odd strings are parsed as strings", {
    expect_equal(
        read_clip_tbl_guess(readr::read_lines(file = "./dates_currency.txt")),
        as.data.frame(tibble::tribble(
                                                 ~date,       ~id,  ~ammount,
                                    "27/10/2016 21:00", "0001234",  "$18.50",
                                    "28/10/2016 21:05", "0001235", "-$18.50"
                                    )
                      )
    )
})

test_that("All delimeters work for parsing as table", {
    expect_equal(read_clip_tbl_guess(readr::read_lines(file = "./pipe_delim.txt")),
                 as.data.frame(tibble::tribble(
                                             ~event,       ~id,
                                             "TYPE1", "01",
                                             "type2,", "02"
                                              )
                 )
    )

    expect_equal(read_clip_tbl_guess(readr::read_lines(file = "./semi_colon_delim.txt")),
                 as.data.frame(tibble::tribble(
                     ~event,       ~id,
                     "TYPE1", "01",
                     "type2,", "02"
                 )
                 )
    )



})

test_that("Table rows with all missing are not ignored", {

    expect_equal(
        read_clip_tbl_guess(readr::read_lines(file = "./tab_with_blank.txt")),
        as.data.frame(tibble::tribble(
                                    ~a,      ~b,
                                    "2",    "hi",
                                    "3", "there",
                                     NA,      NA,
                                    "4",  "robot"
                                    )

                      )
        )
})

test_that("Brisbane Weather with empty lines has separator guessed as tab.", {

  expect_equal(
    guess_sep(readr::read_lines(file = "./brisbane_weather_empty_lines.txt")),
    "\t"
    )
})

test_that("Brisbane Weather with empty lines is parsed, types are guessed, rendered and then can be parsed by R correctly as a tibble", {
  skip_if_not(is_clipr_available, skip_msg)
  skip_on_cran()
  skip_if_not(rstudioapi::isAvailable())
  expect_equal(
    {clipr::write_clip(readr::read_lines(file = "./brisbane_weather_empty_lines.txt"))
    eval(parse(text = paste0("tibble::",tribble_paste())))},
    {tibble::tribble(
      ~X,          ~Location, ~Min, ~Max,
      "Partly cloudy.",         "Brisbane",   19,   29,
      "Partly cloudy.", "Brisbane Airport",   18,   27,
      "Possible shower.",       "Beaudesert",   15,   30,
      "Partly cloudy.",        "Chermside",   17,   29,
      "Shower or two. Possible storm.",           "Gatton",   15,   32,
      "Possible shower.",          "Ipswich",   15,   30,
      "Partly cloudy.",    "Logan Central",   18,   29,
      "Mostly sunny.",            "Manly",   20,   26,
      "Partly cloudy.",    "Mount Gravatt",   17,   28,
      "Possible shower.",            "Oxley",   17,   30,
      "Partly cloudy.",        "Redcliffe",   19,   27
    )}
  )
})

