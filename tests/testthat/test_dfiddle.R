context("test dfiddle & toggle_quote")

test_that("naked vectors are identified",{
  expect_true(is_naked_vec("1 2 3"))
  expect_true(is_naked_vec("  1 2 3  "))
  expect_true(is_naked_vec("1 \n 2 \n 3"))
  expect_true(is_naked_vec("'word' \"word\" word"))
  expect_true(is_naked_vec("'word', \"word\", word"))
  expect_true(is_naked_vec("'word',\n\"word\",\nword"))
  expect_true(is_naked_vec("'word'\n\"word\"\nword"))
})

test_that("horizontal vectors are identified",{
  expect_true(is_horiz_vec("c(1, 2, 3)"))
  expect_true(is_horiz_vec("c( 1  , 2     , 3     )"))
  expect_true(is_horiz_vec("     c(1, 2, 3)     "))
})
