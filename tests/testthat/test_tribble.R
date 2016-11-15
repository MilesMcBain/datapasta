context("test tribble paste")

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



