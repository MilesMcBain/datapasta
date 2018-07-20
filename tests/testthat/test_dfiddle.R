context("test dfiddle & toggle_quote")

test_that("naked vectors are identified",{
  expect_true(is_naked_vec("1 2 3"))
  expect_true(is_naked_vec("  1 2 3  "))
  expect_true(is_naked_vec("1 \n 2 \n 3"))
  expect_true(is_naked_vec("'word' \"word\" word"))
  expect_true(is_naked_vec("'word', \"word\", word"))
  expect_true(is_naked_vec("'word',\n\"word\",\nword"))
  expect_true(is_naked_vec("'word'\n\"word\"\nword"))
  expect_true(is_naked_vec("'word', \"word\" \nword"))
})

test_that("horizontal vectors are identified",{
  expect_true(is_horiz_vec("c(1, 2, 3)"))
  expect_true(is_horiz_vec("c( 1  , 2     , 3     )"))
  expect_true(is_horiz_vec("     c(1, 2, 3)     "))
  expect_true(is_horiz_vec("  c( 'word', word,     \"word\" )" ))
  expect_false(is_horiz_vec("c( 'word',\nword,\n\"word\" )"))
})

test_that("vertical vectors are identified", {
  expect_true(is_vert_vec("c(1,\n2,\n3)"))
  expect_true(is_vert_vec("c(1,\n\n2,\n3)"))
  expect_true(is_vert_vec("  c(1 ,\n2,\n3)"))
  expect_true(is_vert_vec("  c(1 ,\n 2,\n3)  "))
  expect_true(is_vert_vec("  c(1 ,\n 2,  \n3)  "))
  expect_true(is_vert_vec("  c( 'word' ,\n word,  \n'word')  "))
  expect_true(is_vert_vec("  c(\n 'word' ,\n word,  \n'word')  "))
  expect_false(is_vert_vec("c(1,2,3)"))
})

test_that("naked vectors are split correctly",{
  expect_equal(split_naked_vec("1 2 3"), c("1","2","3"))
  expect_equal(split_naked_vec("  1 2 3  "), c("1","2","3"))
  expect_equal(split_naked_vec("'word', \"word\" \nword"), c("'word'","\"word\"","word"))
  expect_equal(split_naked_vec("'word'  ,  \n \"word\" \nword"), c("'word'","\"word\"","word"))
})

test_that("horizontal vectors are split correctly",{
 expect_equal(split_horiz_vec("c(1, 2, 3)"), c("1","2","3"))
 expect_equal(split_horiz_vec("     c(1,  2, 3)     "), c("1","2","3"))
 expect_equal(split_horiz_vec("  c( 'word', word,     \"word\" )"), c("'word'","word","\"word\""))
 expect_equal(split_horiz_vec("c('word',word,\"word\")"), c("'word'","word","\"word\""))
})

test_that("vertical vectors are split correctly",{
  expect_equal(split_vert_vec("c(1,\n2,\n3)"),c("1","2","3"))
  expect_equal(split_vert_vec("c(1,\n\n\n2,\n3)"),c("1","2","3"))
  expect_equal(split_vert_vec("c(\n 1,\n  2,\n3 )"),c("1","2","3"))
  expect_equal(split_vert_vec("  c( 'word' ,\n word,  \n\"word\")  "),c("'word'","word","\"word\""))
  expect_equal(split_vert_vec("  c( 'word' ,\n word,  \n\"word\"    )  "),c("'word'","word","\"word\""))
})

test_that("number of lines is calculated correctly",{
  expect_equal(n_lines("c(\n 1,\n\n  2,\n3 )"), 5)
  expect_equal(n_lines("c(1,2,3)"), 1)
})

test_that("final line length is calculated correctly",{
  expect_equal(last_line_content_length("c(12\n,34\n 12345678 )"),11)
  expect_equal(last_line_content_length("c(12\n,34\n )"),2)
  expect_equal(last_line_content_length("c(12\n,34\n)"),1)
  expect_equal(last_line_content_length("c(12\n,34\n"),0)
})

test_that("quotes are toggled correctly",{
  expect_equal(toggle_quote_elems(c("'a'","b","1")), c("'a'","\"b\"", "\"1\""))
  expect_equal(toggle_quote_elems(c("'a'","\"b\"", "\"1\"")), c("a","b", "1"))
  expect_equal(toggle_quote_elems(c("a","b", "1")), c("\"a\"","\"b\"", "\"1\""))
  expect_equal(toggle_quote_elems(c("  a  ","b", "1")), c("\"  a  \"","\"b\"", "\"1\""))
})
