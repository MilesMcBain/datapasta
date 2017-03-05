context("test vector paste")
skip_msg <- "System clipboard is not available - skipping test."
is_clipr_available <- clipr::clipr_available()
is_rstudio_available <- rstudioapi::isAvailable()


test_that("A vector of strings is pasted correctly", {
    skip_if_not(is_clipr_available, skip_msg)
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
                    vector_paste()
                 },
                {
                  "c(\"Normal\", \"Steel\", \"Psychic\", \"Dark\", \"Dragon\", \"Normal\", \"Water\", \"Flying\")"
                }


     )
})

test_that("Vector_paste formats numeric correctly ", {
  skip_if_not(is_rstudio_available)
  skip_if_not(is_clipr_available, skip_msg)
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
    vector_paste()
  },
  "c(7L, 8L, 7L, 6L, 6L, NA, 6L, 9L)"
  )
})


test_that("Vector_paste formats combine numeric/strings correctly ", {
    skip_if_not(is_rstudio_available)
    skip_if_not(is_clipr_available, skip_msg)
    skip_on_cran()
    expect_equal({
        suppressWarnings(clipr::write_clip(content = c("a","6", "b", "4"))
        )
        vector_paste()
    },
    "c(\"a\", \"6\", \"b\", \"4\")"
    )
})


test_that("Vector_paste handles empty strings", {
    skip_if_not(is_rstudio_available)
    skip_if_not(is_clipr_available, skip_msg)
    skip_on_cran()
    expect_equal({
    suppressWarnings(clipr::write_clip(content = c("a","6", "", "4"))
    )
    vector_paste()
  },
  "c(\"a\", \"6\", NA, \"4\")"
  )
})

