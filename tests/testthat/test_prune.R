context("prune")

#####################################
# Test the filter generator function
# See test_fast_forward for more prune tests, as the
# setup is cumbersome, and FF does it already.

test_that("Filter generator works", {
  expect_equal(stoner:::get_filters(NULL, NULL, NULL, NULL), "")
  expect_equal(get_filters("A", NULL, NULL, NULL), "WHERE scenario.touchstone IN ('A')")
  expect_equal(get_filters(c("A", "B"), NULL, NULL, NULL), "WHERE scenario.touchstone IN ('A','B')")
  expect_equal(get_filters(NULL, "A", NULL, NULL), "WHERE modelling_group IN ('A')")
  expect_equal(get_filters(NULL, c("A", "B"), NULL, NULL), "WHERE modelling_group IN ('A','B')")
  expect_equal(get_filters(NULL, NULL, "A", NULL), "WHERE disease IN ('A')")
  expect_equal(get_filters(NULL, NULL, c("A", "B"), NULL), "WHERE disease IN ('A','B')")
  expect_equal(get_filters(NULL, NULL, NULL, "A"), "WHERE scenario_description IN ('A')")
  expect_equal(get_filters(NULL, NULL, NULL, c("A", "B")), "WHERE scenario_description IN ('A','B')")
  expect_equal(get_filters("A", "D", NULL, NULL), "WHERE scenario.touchstone IN ('A') AND modelling_group IN ('D')")
  expect_equal(get_filters(NULL, c("A", "B"), c("C", "D"), NULL), "WHERE modelling_group IN ('A','B') AND disease IN ('C','D')")
  expect_equal(get_filters("Z", c("A", "B"), c("C", "D"), NULL), "WHERE scenario.touchstone IN ('Z') AND modelling_group IN ('A','B') AND disease IN ('C','D')")
  expect_equal(get_filters("Z", c("A", "B"), c("C", "D"), "Y"), "WHERE scenario.touchstone IN ('Z') AND modelling_group IN ('A','B') AND disease IN ('C','D') AND scenario_description IN ('Y')")
})

