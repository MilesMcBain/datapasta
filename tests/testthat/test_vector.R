context("test vector paste")

test_that("A vector of strings is parsed correctly", {
    expect_equal({
                    clipr::write_clip(content = c("Normal",
                                                "Steel",
                                                "Psychic",
                                                "Dark",
                                                "Dragon",
                                                "Normal",
                                                "Water",
                                                "Flying"))
                    parse_vector()
                 },
                 structure(c("Normal",
                             "Steel",
                             "Psychic",
                             "Dark",
                             "Dragon",
                             "Normal",
                             "Water",
                             "Flying"),
                            type = "character"
                 )
     )
})

test_that("A vector of numbers is parsed correctly", {
    expect_equal({
        suppressWarnings(clipr::write_clip(content = c(7,
                                                      8,
                                                      7,
                                                      6,
                                                      6,
                                                      6,
                                                      6,
                                                      9))
        )
        parse_vector()
    },
    structure(c("7",
                "8",
                "7",
                "6",
                "6",
                "6",
                "6",
                "9"),
              type = "integer"
    )
    )
})

test_that("Vector_paste formats numeric correctly ", {
  expect_equal({
    suppressWarnings(clipr::write_clip(content = c(7,
                                                   8,
                                                   7,
                                                   6,
                                                   6,
                                                   NaN,
                                                   6,
                                                   9))
    )
    vector_paste()
  },
  "c(7, 8, 7, 6, 6, NaN, 6, 9)"
  )
})


test_that("Vector_paste formats combine numeric/strings correctly ", {
    expect_equal({
        suppressWarnings(clipr::write_clip(content = c("a","6", "b", "4"))
        )
        vector_paste()
    },
    "c(\"a\", \"6\", \"b\", \"4\")"
    )
})


test_that("Vector_paste handles empty strings", {
  expect_equal({
    suppressWarnings(clipr::write_clip(content = c("a","6", "", "4"))
    )
    vector_paste()
  },
  "c(\"a\", \"6\", NA, \"4\")"
  )
})

