context("stochastic_cert")

# Some tests for stochastic model run parameter set certificate validation.
# stone_stochastic_cert_verify <- function(con, certfile, modelling_group,
#                                          touchstone) {

test_that("Bad arguments", {
  test <- new_test()
  standard_disease_touchstones(test)
  standard_responsibility_support(test)
  do_test(test)

  new_file <- tempfile(fileext = ".json")
  dummy <-'[
    {
      "id": 74,
      "disease": "HepB",
      "uploaded_by": "wes",
      "uploaded_on": "2020-07-27T15:14:27.969Z"
    },
    {
      "signature": "sigdata"
    }
  ]'
  writeLines(dummy, new_file)

  expect_error(stoner::stone_stochastic_cert_verify(
    test$con , new_file, "Rudolph", "ivinghoe-beacon-1"),
    "Unknown modelling group: Rudolph")

  expect_error(stoner::stone_stochastic_cert_verify(
    test$con, new_file, "LAP-elf", "ivinghoe-beacon-1"),
    "Unknown touchstone:")

  expect_error(stoner::stone_stochastic_cert_verify(
    test$con, tempfile(), "LAP-elf", "nevis-1"),
    "Stochastic certificate not found")

  expect_error(stoner::stone_stochastic_cert_verify(
    test$con, NULL, "LAP-elf", "nevis-1"),
    "Stochastic certificate not found")
})

test_that("Mismatched certificate arguments", {
  test <- new_test()
  standard_disease_touchstones(test)
  standard_responsibility_support(test)
  resp <- default_responsibility()
  create_responsibilities(test, resp)
  do_test(test)

  new_file <- valid_certificate(test$con)

  expect_error(stoner::stone_stochastic_cert_verify(
    test$con, new_file, "EBHQ-bunny", "nevis-1"),
    "Modelling group mismatch - expected LAP-elf")

  expect_error(stoner::stone_stochastic_cert_verify(
    test$con, new_file, "LAP-elf", "kili-1"),
    "Touchstone mismatch - expected nevis-1")

  expect_invisible(stoner::stone_stochastic_cert_verify(
    test$con, new_file, "LAP-elf", "nevis-1"))
})
