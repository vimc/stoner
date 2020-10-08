context("utils")

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

test_that("mash", {
  car <- mtcars[rownames(mtcars) == 'Volvo 142E', ]
  expect_equal(mash(car), "1\r2\r4\r121\r4.11\r4\r109\r21.4\r18.6\r1\r2.78")
  car2 <- car[, sort(names(car))]
  expect_equal(mash(car), mash(car2))
  expect_equal(mash(car, c("mpg", "disp")), "121\r21.4")
})

test_that("assign_serial_id works", {
  csv <- data_frame(letter = c("A","B","C"), number = c(1,4,7))
  db <- data_frame(letter = c("C", "D","E"), number = c(7,8,9), id = c(1,2,3))
  csv2 <- assign_serial_ids(csv,db)
  csv2 <- csv2[order(csv2$id), ]
  expect_equal(csv2$id, c(-2, -1, 1))
  expect_equal(csv2$already_exists_db, c(FALSE, FALSE, TRUE))
})

test_that("null_or_empty works", {
  expect_true(null_or_empty(NULL))
  expect_true(null_or_empty(mtcars[mtcars$mpg>99999, ]))
})

test_that("Asserts", {
  expect_error(assert_nonmissing(NA), "'NA' must not be NA")
  expect_invisible(assert_nonmissing(c(1, 2, 3)))

  expect_error(assert_scalar(c(1,2)), "(.*) must be a scalar")
  expect_invisible(assert_scalar(123))

  expect_error(assert_character(123), "(.*) must be a character")
  expect_invisible(assert_character(c("1","2")))

  expect_error(assert_scalar_character(c("1","2"), "(.*) must be a scalar"))
  expect_error(assert_scalar_character(123), "(.*) must be a character")
  expect_invisible(assert_scalar_character("a"))
  expect_error(assert_scalar_character(""), "(.*) must be nonempty")

  expect_error(assert_connection(mtcars), "(.*) must be a PqConnection object")
  test <- new_test()
  expect_invisible(assert_connection(test$con))

  expect_error(assert_scalar_logical(c("FALSE", "TRUE")), "'c\\(\"FALSE\", \"TRUE\")' must be a logical")
  expect_error(assert_scalar_logical(c(TRUE, FALSE)), "'c\\(TRUE, FALSE)' must be a scalar")
  expect_error(assert_scalar_logical("FALSE"), "'\"FALSE\"' must be a logical")
  expect_invisible(assert_scalar_logical(FALSE))

})
