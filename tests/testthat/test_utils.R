context(utils)

test_that("%||% works", {
  expect_equal(NULL %||% 0, 0)
  expect_equal(1 %||% 0, 1)
})

test_that("sql_in works", {
  expect_equal(sql_in(c(1,2,3)), "(1,2,3)")
  expect_equal(sql_in(c("a","b","c")), "('a','b','c')")
  expect_error(sql_in(NULL),
               "Can't convert things with sql_in")
})
