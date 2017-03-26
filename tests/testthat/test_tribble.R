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
    eval(parse(text = tribble_construct()))},
    {tibble::tribble(
      ~X,          ~Location, ~Min, ~Max,
      "Partly cloudy.",         "Brisbane",   19L,   29L,
      "Partly cloudy.", "Brisbane Airport",   18L,   27L,
      "Possible shower.",       "Beaudesert",   15L,   30L,
      "Partly cloudy.",        "Chermside",   17L,   29L,
      "Shower or two. Possible storm.",           "Gatton",   15L,   32L,
      "Possible shower.",          "Ipswich",   15L,   30L,
      "Partly cloudy.",    "Logan Central",   18L,   29L,
      "Mostly sunny.",            "Manly",   20L,   26L,
      "Partly cloudy.",    "Mount Gravatt",   17L,   28L,
      "Possible shower.",            "Oxley",   17L,   30L,
      "Partly cloudy.",        "Redcliffe",   19L,   27L
    )}
  )
})

test_that("Data with all rows ending in commas (empty final column) has separator guessed correctly", {
  skip_if_not(is_clipr_available, skip_msg)
  skip_on_cran()
  skip_if_not(rstudioapi::isAvailable())
  expect_equal(
    {clipr::write_clip(readr::read_lines(file = "./empty_final_col_comma.txt"))
      suppressWarnings(eval(parse(text = tribble_construct())))}, #Will generate a warning about all NA to max
    {tibble::tribble(
        ~a, ~b, ~c,
         1L,  2L, NA,
         3L,  4L, NA,
         5L,  6L, NA

    )}
  )
})

test_that("Data with a comma decimal mark can be parsed correctly", {
  skip_if_not(is_clipr_available, skip_msg)
  skip_on_cran()
  dp_set_decimal_mark(",")
  on.exit(dp_set_decimal_mark("."))
  expect_equal(
    {clipr::write_clip(readr::read_lines(file = "./comma_delim.txt"))
      eval(parse(text = tribble_construct()))},
    {tibble::tribble(
      ~A,    ~B,  ~C,   ~D,
      3L,   7.4,  5L,   5L,
      5L,     9,  8L,   5L,
      10L,     9,  3L,  10L,
      2L,     7,  9L,   5L,
      10L,     7,  2L,   7L,
      10L,    10,  2L,  10L,
      1L,     7,  4L,   9L
    )}
  )
})

test_that("The decimal mark is returned to .", {
  expect_equal(
    {.global_datapasta_env$decimal_mark},
    {"."}
  )
})

test_that("tribble_paste() input data.framess can render correctly as tribbles",
          {
            expect_equal(
              {
                eval( parse(text = tribble_construct(datasets::airquality[1:6,])) )
              },
              {
                tibble::as_tibble(datasets::airquality[1:6,])
              }
            )
          })

test_that("Input tibbles with basic char, int, double render as tibbles transparently",
          {
            expect_equal(
              {
                eval( parse(text = tribble_construct(tibble::tribble(
                  ~char, ~int,  ~dbl,
                  "a",   1L,   1.1,
                  "b",   3L,   3.3
                ))) )
              },
              {
                tibble::tribble(
                  ~char, ~int,  ~dbl,
                  "a",   1L,   1.1,
                  "b",   3L,   3.3
                )
              }
            )
          })

test_that("Attempting to input non-table generates a message",
{
  suppressWarnings(
    expect_message(tribble_construct(as.list(datasets::mtcars)),"Could not format input_table as table")
  )
})

test_that("Quotes \' in input are escaped",
          {
            skip_if_not(is_clipr_available, skip_msg)
            expect_equal({
              clipr::write_clip("a,b,c\nthis,is,testing\nnow,you\'re,testing")
              eval(parse(text = tribble_construct()))
            },
            {
              tibble::tribble(
                ~a,        ~b,         ~c,
                "this",      "is",  "testing",
                "now", "you\'re",  "testing"
              )
            })}
)

test_that("Quotes \" in input are escaped",
          {
            skip_if_not(is_clipr_available, skip_msg)
            expect_equal({
              clipr::write_clip("a,b,c\nthis,is,testing\nnow,you\"re,testing")
              eval(parse(text = tribble_construct()))
              },
              {
              tibble::tribble(
                ~a,        ~b,         ~c,
                "this",      "is",  "testing",
                "now",  "you\"re",  "testing"
              )}
             )
          }
)
test_that("Attempting to input a large table generates a message",
{
  suppressWarnings(
    expect_message(tribble_construct(data.frame(col1 = seq_len(400))),"Supplied large input_table")
  )
})
