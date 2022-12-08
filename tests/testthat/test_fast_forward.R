context("fast_forward")

# Here, we do tests on the fast_forward.csv file

# Cols: modelling_group, touchstone_from, touchstone_to, scenario
#
# where modelling_group and scenario can be * for wildcard
# or a semi-colon list of modelling_groups or scenario_descriptions

test_that("Empty ff csv extracts nothing", {

  # No CSV provided

  x <- extract_fast_forward(list(), NULL, NULL)
  expect_true(is.null(x))

  # CSV but no rows. How do we create a data frame with no rows?

  csv <- data.frame(modelling_group = "", touchstone_from = "",
                    touchstone_to = "", scenario = "")
  csv <- csv[csv$modelling_group %in% "Jethro Tull", ]

  x <- extract_fast_forward(list(fast_forward_csv = csv, NULL, NULL))
  expect_true(is.null(x))
})

write_ff_csv <- function(test, ...) {
  df <- ff_csv(...)
  write.csv(df, file.path(test$path, "meta", "fast_forward.csv"),
                          row.names = FALSE)
}

ff_csv <- function(modelling_group, scenario, touchstone_from, touchstone_to) {
  data.frame(stringsAsFactors = FALSE,
             modelling_group = modelling_group,
             scenario = scenario,
             touchstone_from = touchstone_from,
             touchstone_to = touchstone_to)
}

test_that("FF CSV content / cannot coexist with other CSVs", {
  test <- new_test()
  write.csv(data.frame(wrong = "mushrooms"),
            file.path(test$path, "meta", "fast_forward.csv"))
  expect_error(do_test(test), "Incorrect columns in fast_forward.csv")

  write_ff_csv(test, "*", "*", "t1", "t2")
  write.csv(ff_csv("*", "*", "t1", "t2"),
            file.path(test$path, "meta", "touchstone.csv"),
            row.names = FALSE)
  expect_error(do_test(test),
    "fast_forward.csv, if specified, must be the only csv")

})

test_that("FF CSV with incorrect modelling_group / scenario / touchstone", {
  test <- new_test()
  standard_disease_touchstones(test)
  standard_responsibility_support(test)
  do_test(test)

  # Invalid touchstone

  expect_error(
    extract_fast_forward(list(
      fast_forward_csv = ff_csv("*", "*", "spud-1", "spud-2")), "", test$con),
    "Required touchstone\\(s) not found: spud-1, spud-2")

  expect_error(
    extract_fast_forward(list(
      fast_forward_csv = ff_csv("*", "*", "kili-1", "spud-2")), "", test$con),
    "Required touchstone\\(s) not found: spud-2")

  expect_error(
    extract_fast_forward(list(
      fast_forward_csv = ff_csv("*", "*", "spud-1", "kili-1")), "", test$con),
    "Required touchstone\\(s) not found: spud-1")

  # Invalid modelling_group

  expect_error(
    extract_fast_forward(list(
      fast_forward_csv = ff_csv("LAP-spud", "*", "kili-1", "nevis-1")), "", test$con),
    "Modelling group\\(s) not found: LAP-spud")

  expect_error(
    extract_fast_forward(list(
      fast_forward_csv = ff_csv(c("LAP-elf", "LAP-spud"), "*", "kili-1", "nevis-1")), "", test$con),
    "Modelling group\\(s) not found: LAP-spud")

  expect_error(
    extract_fast_forward(list(
      fast_forward_csv = ff_csv("LAP-elf;LAP-spud", "*", "kili-1", "nevis-1")), "", test$con),
    "Modelling group\\(s) not found: LAP-spud")

  # Invalid scenario

  expect_error(
    extract_fast_forward(list(
      fast_forward_csv = ff_csv("LAP-elf", "hot_potato", "kili-1", "nevis-1")), "", test$con),
    "Touchstone-scenario\\(s) not found: kili-1:hot_potato, nevis-1:hot_potato")

  expect_error(
    extract_fast_forward(list(
      fast_forward_csv = ff_csv("LAP-elf", "hot_potato;hot_chocolate", "kili-1", "nevis-1")), "", test$con),
    "Touchstone-scenario\\(s) not found: kili-1:hot_potato, kili-1:hot_chocolate, nevis-1:hot_potato")

  expect_error(
    extract_fast_forward(list(
      fast_forward_csv = ff_csv("LAP-elf", c("hot_potato", "hot_chocolate"),
                                "kili-1", "nevis-1")), "", test$con),
    "Touchstone-scenario\\(s) not found: kili-1:hot_potato, kili-1:hot_chocolate, nevis-1:hot_potato")
})

test_that("Fast-Forward tests", {

  # There is a hefty amount of support needed for adding
  # burden estimate sets and testing fast-forward/prune

  # Set up some touchstones, groups and scenarios - but can't use
  # stoner to do it, as FF needs to be a standalone thing.

  test <- init_for_ff_prune()

  expect_error(
    extract_fast_forward(list(
      fast_forward_csv = ff_csv("LAP-elf",
                                c("hot_chocolate"),
                                c("kili-1", "kili-2"),
                                c("kili-2", "nevis-1"))), "", test$con),
    "Same touchstone appears in both touchstone_to and touchstone_from.")



  # Dummy expectations (which will, naughtily, share between everyone)

  expec <- dummy_expectation(test)

  # The rest will all be do with with responsibility_set
  # and responsibility, so...

  get_responsibility_set <- function(modelling_group, touchstone) {
    DBI::dbGetQuery(test$con, "
    SELECT id FROM responsibility_set
     WHERE modelling_group = $1
       AND touchstone = $2", list(modelling_group, touchstone))$id
  }

  get_responsibilities <- function(responsibility = NA, responsibility_set = NA) {
    if (!is.na(responsibility_set)) {
      DBI::dbGetQuery(test$con, "
        SELECT * FROM responsibility WHERE responsibility_set IN ($1)",
                      paste(responsibility_set, collapse = ","))
    } else {
      DBI::dbGetQuery(test$con, "
        SELECT * FROM responsibility WHERE id IN ($1)",
                      paste(responsibility, collapse = ","))
    }
  }

  add_comment <- function(id, comment, type, date) {
    DBI::dbGetQuery(test$con, sprintf("
      INSERT INTO %s_comment (%s, comment, added_by, added_on)
           VALUES ($1, $2, $3, $4) RETURNING id", type, type),
                    list(id, comment, "Elf", date))$id
  }

  add_comment_rs <- function(id, comment, date) {
    add_comment(id, comment, "responsibility_set", date)
  }

  add_comment_r <- function(id, comment, date) {
    add_comment(id, comment, "responsibility", date)
  }

  # Get comment for the responsibility set

  get_comment <- function(id, type) {
    DBI::dbGetQuery(test$con, sprintf("
      SELECT CONCAT(comment,'\r',%s) AS result FROM %s_comment
       WHERE id = $1", type, type), id)$result
  }

  get_comment_rs <- function(id) {
    get_comment(id, "responsibility_set")
  }

  get_comment_r <- function(id) {
    get_comment(id, "responsibility")
  }

  no_neg_ids <- function() {
    pos_or_na <- function(table) {
      count <- DBI::dbGetQuery(test$con,
                               sprintf("SELECT COUNT(*) FROM %s", table))$count
      if (count == 0) return(0)
      DBI::dbGetQuery(test$con, sprintf("SELECT MIN(id) FROM %s", table))$min
    }

    expect_gte(pos_or_na("responsibility_set"), 0)
    expect_gte(pos_or_na("responsibility"), 0)
    expect_gte(pos_or_na("responsibility_comment"), 0)
    expect_gte(pos_or_na("responsibility_set_comment"), 0)
  }


  # And finally we are ready to do proper tests.

  ###################################################################
  # 1. Single group.
  #    Responsibility_set (and hence responsibility) doesn't exist
  #    Single scenario
  #    Model group and scenario named explicitly in file
  ###################################################################
  test1 <- function() {
    clear_test_resps(test)
    resp_set <- add_responsibility_set(test, "LAP-elf", "nevis-1")
    resp <- add_responsibility(test, "nevis-1", resp_set, "hot_chocolate", expec)
    bes <- add_burden_estimate_set(test, resp)
    add_burden_estimates(test, pathetic_data(test), bes, resp)
    write_ff_csv(test, "LAP-elf", "hot_chocolate", "nevis-1", "nevis-2")

    # To be sure, the 2nd responsibility_set doesn't exist.

    expect_equal(0, length(get_responsibility_set("LAP-Elf", "nevis-2")))

    ##########################################################
    # Add some comments for the responsibility / set


    rs_c1 <- add_comment_rs(resp_set, "A partridge in a pear-tree", as.Date("2021-01-01"))
    rs_c2 <- add_comment_rs(resp_set, "Two turtle doves", as.Date("2021-01-02"))
    r_c1 <- add_comment_r(resp, "Three French hens", as.Date("2021-01-03"))
    r_c2 <- add_comment_r(resp, "Four colly birds", as.Date("2021-01-04"))

    # Run the FF import.

    do_test(test)

    # We expect:
    #   1. a new responsibility_set,

    new_rset <- get_responsibility_set("LAP-elf", "nevis-2")
    expect_equal(1, length(new_rset))

    #   2. a new responsibility for that set

    new_resps <- get_responsibilities(responsibility_set = new_rset)
    expect_equal(1, nrow(new_resps))

    #   3. That resp.  has a current_burden_estimate_set matching earlier bes
    expect_equal(new_resps$current_burden_estimate_set, bes)

    #   4. Old responsibility is still there, with NA for
    #      current_burden_estimate_set

    old_resp <- get_responsibilities(responsibility = resp)
    expect_true(is.na(old_resp$current_burden_estimate_set))

    #   5. Comments... expect original comments to remain.

    expect_equal(get_comment_rs(rs_c1),
                paste("A partridge in a pear-tree", resp_set, sep = '\r'))
    expect_equal(get_comment_rs(rs_c2),
                paste("Two turtle doves", resp_set, sep = '\r'))
    expect_equal(get_comment_r(r_c1),
                paste("Three French hens", resp, sep = '\r'))
    expect_equal(get_comment_r(r_c2),
                paste("Four colly birds", resp, sep = '\r'))

    # Except one modified comment in the new resp_set and new responsibility

    expect_equal(DBI::dbGetQuery(test$con,
      "SELECT COUNT(*) FROM responsibility_set_comment
                      WHERE responsibility_set = $1", new_rset)$count, 1)

    expect_equal(DBI::dbGetQuery(test$con,
      "SELECT COUNT(*) FROM responsibility_comment
                      WHERE responsibility = $1", new_resps$id)$count, 1)

    expect_equal(DBI::dbGetQuery(test$con,
      "SELECT comment FROM responsibility_set_comment
                     WHERE responsibility_set = $1", new_rset)$comment,
               "Two turtle doves - Fast-forwarded from nevis-1")

    expect_equal(DBI::dbGetQuery(test$con,
      "SELECT comment FROM responsibility_comment
                     WHERE responsibility = $1", new_resps$id)$comment,
               "Four colly birds - Fast-forwarded from nevis-1")

    no_neg_ids()
  }

  test1()

  ###################################################################
  # 2. Two out of three groups, same scenario
  #    Test with semi-colon in modelling-group, and also
  #    separate lines.
  ###################################################################

  test2 <- function(pass) {
    clear_test_resps(test)
    resp_set_a1 <- add_responsibility_set(test, "LAP-elf", "nevis-1")
    resp_set_b1 <- add_responsibility_set(test, "R-deer", "nevis-1")
    resp_a1 <- add_responsibility(test, "nevis-1", resp_set_a1, "hot_chocolate", expec)
    resp_b1 <- add_responsibility(test, "nevis-1", resp_set_b1, "hot_chocolate", expec)
    bes_a1 <- add_burden_estimate_set(test, resp_a1)
    bes_b1 <- add_burden_estimate_set(test, resp_b1)
    add_burden_estimates(test, pathetic_data(test), bes_a1, resp_a1)
    add_burden_estimates(test, pathetic_data(test), bes_b1, resp_b1)
    if (pass == 1) {
      write_ff_csv(test, "LAP-elf;R-deer", "hot_chocolate", "nevis-1", "nevis-2")
    } else if (pass == 2) {
      write_ff_csv(test, c("LAP-elf", "R-deer"), "hot_chocolate", "nevis-1", "nevis-2")
    }
    do_test(test)
    expect_true(is.na(get_responsibilities(responsibility = resp_a1)$current_burden_estimate_set))
    expect_true(is.na(get_responsibilities(responsibility = resp_b1)$current_burden_estimate_set))
    new_resp_set_a1 <- get_responsibility_set("LAP-elf", "nevis-2")
    new_resp_set_b1 <- get_responsibility_set("R-deer", "nevis-2")
    expect_equal(bes_a1, get_responsibilities(responsibility_set = new_resp_set_a1)$current_burden_estimate_set)
    expect_equal(bes_b1, get_responsibilities(responsibility_set = new_resp_set_b1)$current_burden_estimate_set)
    no_neg_ids()

  }

  test2(1)
  test2(2)

  ###################################################################
  # Multiple groups, multiple scenarios
  # Test *, semi-colon, and multi-line.
  ###################################################################

  test3 <- function(pass) {
    clear_test_resps(test)
    resp_set_a1 <- add_responsibility_set(test, "LAP-elf", "nevis-1")
    resp_set_b1 <- add_responsibility_set(test, "R-deer", "nevis-1")
    resp_a1 <- add_responsibility(test, "nevis-1", resp_set_a1, "hot_chocolate", expec)
    resp_b1 <- add_responsibility(test, "nevis-1", resp_set_b1, "pies", expec)
    bes_a1 <- add_burden_estimate_set(test, resp_a1)
    bes_b1 <- add_burden_estimate_set(test, resp_b1)
    add_burden_estimates(test, pathetic_data(test), bes_a1, resp_a1)
    add_burden_estimates(test, pathetic_data(test), bes_b1, resp_b1)

    if (pass %in% c(1,4,7)) ts <- "LAP-elf;R-deer"
    if (pass %in% c(2,5,8)) ts <- c("LAP-elf", "R-deer")
    if (pass %in% c(3,6,9)) ts <- "*"
    if (pass %in% c(1,2,3)) sc <- "hot_chocolate;pies"
    if (pass %in% c(4,5,6)) sc <- c("hot_chocolate", "pies")
    if (pass %in% c(7,8,9)) sc <- "*"

    write_ff_csv(test, ts, sc, "nevis-1", "nevis-2")
    do_test(test)
    expect_true(is.na(get_responsibilities(responsibility = resp_a1)$current_burden_estimate_set))
    expect_true(is.na(get_responsibilities(responsibility = resp_b1)$current_burden_estimate_set))
    new_resp_set_a1 <- get_responsibility_set("LAP-elf", "nevis-2")
    new_resp_set_b1 <- get_responsibility_set("R-deer", "nevis-2")
    expect_equal(bes_a1, get_responsibilities(responsibility_set = new_resp_set_a1)$current_burden_estimate_set)
    expect_equal(bes_b1, get_responsibilities(responsibility_set = new_resp_set_b1)$current_burden_estimate_set)
    no_neg_ids()
  }

  for (i in 1:9) test3(i)

  #########################################################################
  # A group where there is an existing responsibility_set in the new
  # touchstone, but no new responsiblity yet.

  test4 <- function(pass) {
    clear_test_resps(test)
    resp_set_a1 <- add_responsibility_set(test, "LAP-elf", "nevis-1")
    resp_set_a2 <- add_responsibility_set(test, "LAP-elf", "nevis-2")
    resp_a1 <- add_responsibility(test, "nevis-1", resp_set_a1, "hot_chocolate", expec)
    bes_a1 <- add_burden_estimate_set(test, resp_a1)
    add_burden_estimates(test, pathetic_data(test), bes_a1, resp_a1)
    rsc1 <- add_comment_rs(resp_set_a1, "Five gold rings", as.Date("2021-01-01"))
    rc1 <- add_comment_r(resp_a1, "Six dplyers dplying", as.Date("2021-01-01"))
    write_ff_csv(test, "*", "*", "nevis-1", "nevis-2")
    old_resp1 <- get_responsibilities(responsibility_set = resp_set_a1)
    old_resp2 <- get_responsibilities(responsibility_set = resp_set_a2)
    do_test(test)

    # We're expecting the bes to be fast-forwarded to the second touchstoe
    # as before - just without having to create the responsibility_set

    new_resp1 <- get_responsibilities(responsibility_set = resp_set_a1)
    new_resp2 <- get_responsibilities(responsibility_set = resp_set_a2)
    expect_equal(1, nrow(old_resp1))
    expect_equal(0, nrow(old_resp2))
    expect_true(is.na(new_resp1$current_burden_estimate_set))
    expect_equal(bes_a1, new_resp2$current_burden_estimate_set)

    # Responsibility set comment should not have moved

    expect_equal(1, as.integer(DBI::dbGetQuery(test$con, "
      SELECT COUNT(*) FROM responsibility_set_comment
                     WHERE responsibility_set = $1", resp_set_a1)$count))

    expect_equal("Five gold rings", DBI::dbGetQuery(test$con,
      "SELECT comment FROM responsibility_set_comment")$comment)

    expect_equal(0, as.integer(DBI::dbGetQuery(test$con, "
      SELECT COUNT(*) FROM responsibility_set_comment
                     WHERE responsibility_set = $1", resp_set_a2)$count))

    # Responsibility comment should have been copied updated.

    expect_equal(1, as.integer(DBI::dbGetQuery(test$con, "
      SELECT COUNT(*) FROM responsibility_comment
                     WHERE responsibility = $1", resp_a1)$count))

    expect_equal("Six dplyers dplying", DBI::dbGetQuery(test$con,
      "SELECT comment FROM responsibility_comment
        WHERE responsibility = $1", resp_a1)$comment)

    expect_equal(1, as.integer(DBI::dbGetQuery(test$con, "
      SELECT COUNT(*) FROM responsibility_comment
                     WHERE responsibility = $1", new_resp2$id)$count))

    expect_equal("Six dplyers dplying - Fast-forwarded from nevis-1",
      DBI::dbGetQuery(test$con,
       "SELECT comment FROM responsibility_comment
        WHERE responsibility = $1", new_resp2$id)$comment)
    no_neg_ids()
  }

  test4()

  #########################################################
  # Now where responsibility_set exists, so does responsibility,
  # but current_burden_estimate_set for the new responsibility
  # is NA. Should slot into existing responsibility

  test5 <- function(pass) {
    clear_test_resps(test)
    resp_set_a1 <- add_responsibility_set(test, "LAP-elf", "nevis-1")
    resp_set_a2 <- add_responsibility_set(test, "LAP-elf", "nevis-2")
    resp_a1 <- add_responsibility(test, "nevis-1", resp_set_a1, "hot_chocolate", expec)
    resp_a2 <- add_responsibility(test, "nevis-2", resp_set_a2, "hot_chocolate", expec)
    bes_a1 <- add_burden_estimate_set(test, resp_a1)
    add_burden_estimates(test, pathetic_data(test), bes_a1, resp_a1)

    # So we should have a non-NA, and an NA bes...

    expect_true(!is.na(get_responsibilities(responsibility = resp_a1)$current_burden_estimate_set))
    expect_true(is.na(get_responsibilities(responsibility = resp_a2)$current_burden_estimate_set))

    rsc1 <- add_comment_rs(resp_set_a1, "Seven farmers farting", as.Date("2021-01-01"))
    rc1 <- add_comment_r(resp_a1, "Eight coders coding", as.Date("2021-01-02"))
    write_ff_csv(test, "*", "*", "nevis-1", "nevis-2")

    old_resp1 <- get_responsibilities(responsibility_set = resp_set_a1)
    old_resp2 <- get_responsibilities(responsibility_set = resp_set_a2)

    do_test(test)

    new_resp1 <- get_responsibilities(responsibility_set = resp_set_a1)
    new_resp2 <- get_responsibilities(responsibility_set = resp_set_a2)

    # Should still be only 2 responsibilities

    expect_equal(2, as.integer(DBI::dbGetQuery(test$con,
      "SELECT COUNT(*) FROM responsibility")$count))

    # BES should have moved to existing responsibility.

    expect_true(is.na(new_resp1$current_burden_estimate_set))
    expect_true(!is.na(new_resp2$current_burden_estimate_set))
    expect_equal(new_resp2$id, old_resp2$id)

    # No new resp sets/responsibility should have been created

    expect_equal(1, nrow(old_resp1))
    expect_equal(1, nrow(old_resp2))
    expect_equal(1, nrow(new_resp1))
    expect_equal(1, nrow(new_resp2))

    # Should still be one responsibility_set comment, the same as before

    expect_equal(rsc1, DBI::dbGetQuery(test$con, "
      SELECT id FROM responsibility_set_comment")$id)

    # Should be two responsibility comments.

    new_rcs <- DBI::dbGetQuery(test$con, "SELECT * FROM responsibility_comment")
    expect_equal(nrow(new_rcs), 2)
    new_rcs <- new_rcs[new_rcs$id != rc1, ]
    expect_equal(nrow(new_rcs), 1)
    expect_equal(new_rcs$responsibility, new_resp2$id)
    expect_equal(new_rcs$comment, "Eight coders coding - Fast-forwarded from nevis-1")
    no_neg_ids()
  }
  test5()

  #########################################################
  # Now where responsibility_set exists, so does responsibility,
  # and current_burden_estimate_set already uploaded.
  # Nothing should happen, and we should get a message saying
  # we already have data in the destination, so we can't
  # fast-forward.

  test6 <- function() {
    clear_test_resps(test)
    resp_set_a1 <- add_responsibility_set(test, "LAP-elf", "nevis-1")
    resp_set_a2 <- add_responsibility_set(test, "LAP-elf", "nevis-2")
    resp_a1 <- add_responsibility(test, "nevis-1", resp_set_a1, "hot_chocolate", expec)
    resp_a2 <- add_responsibility(test, "nevis-2", resp_set_a2, "hot_chocolate", expec)
    bes_a1 <- add_burden_estimate_set(test, resp_a1)
    bes_a2 <- add_burden_estimate_set(test, resp_a1)
    add_burden_estimates(test, pathetic_data(test), bes_a1, resp_a1)
    add_burden_estimates(test, pathetic_data(test), bes_a2, resp_a2)
    rsc1 <- add_comment_rs(resp_set_a1, "Nine numpties numping", as.Date("2021-01-01"))
    rc1 <- add_comment_r(resp_a1, "Ten shrubbers shrubbing", as.Date("2021-01-02"))
    write_ff_csv(test, "*", "*", "nevis-1", "nevis-2")

    old_resp1 <- get_responsibilities(responsibility_set = resp_set_a1)
    old_resp2 <- get_responsibilities(responsibility_set = resp_set_a2)

    expect_message(do_test(test),
      "Estimates already found in target touchstone for:")

    new_resp1 <- get_responsibilities(responsibility_set = resp_set_a1)
    new_resp2 <- get_responsibilities(responsibility_set = resp_set_a2)

    expect_true(identical(new_resp1, old_resp1))
    expect_true(identical(new_resp2, old_resp2))
  }

  test6()

  # Consistency checks - these errors should never occur, so will have to
  # manually corrupt the database to emulate them.

  test_consistency <- function(pass) {
    clear_test_resps(test)
    resp_set_a1 <- add_responsibility_set(test, "LAP-elf", "nevis-1")
    resp_set_b1 <- add_responsibility_set(test, "R-deer", "nevis-1")
    resp_a1 <- add_responsibility(test, "nevis-1", resp_set_a1, "hot_chocolate", expec)
    resp_b1 <- add_responsibility(test, "nevis-1", resp_set_b1, "hot_chocolate", expec)
    bes_a1 <- add_burden_estimate_set(test, resp_a1)
    bes_b1 <- add_burden_estimate_set(test, resp_b1)
    add_burden_estimates(test, pathetic_data(test), bes_a1, resp_a1)
    add_burden_estimates(test, pathetic_data(test), bes_b1, resp_b1)

    # Muptiple responsibilities with same current_bes

    if (pass == 1) {
      bes_id <- DBI::dbGetQuery(test$con, "SELECT id FROM burden_estimate_set SET LIMIT 1")$id
      DBI::dbExecute(con, "UPDATE responsibility SET current_burden_estimate_set = $1", bes_id)
      expect_error(stoner::check_ff_consistency(test$con),
                   "Duplicate current_burden_estimate_set")

    # scenario touchstone different from responsibility_set touchstone in
    # same responsibility

    } else if (pass == 2) {
      rset <- DBI::dbGetQuery(test$con, "SELECT * FROM responsibility_set
                                    WHERE touchstone = 'nevis-1' LIMIT 1")
      DBI::dbExecute(test$con, "UPDATE responsibility_set SET touchstone = 'nevis-2'
                                WHERE id = $1", rset$id)
      expect_error(stoner::check_ff_consistency(test$con),
                   "Inconsistent scenario/responsibility_set")


    } else if (pass == 3) {
      resp <- DBI::dbGetQuery(test$con, "SELECT * FROM responsibility LIMIT 1")
      DBI::dbExecute(test$con,"UPDATE responsibility
                              SET scenario = $1", resp$scenario + 1)
      expect_error(stoner::check_ff_consistency(test$con),
                   "Inconsistent scenario/responsibility_set")

    # burden_estimate_set->responsibility mismatch
    # responsibility->current_burden_estimate_set

    }

    test_consistency(1:3)

    # check detection of inconsistency with
    # responsibility->current_burden_estimate_set->responsibility

    test_bes_consistency <- function() {
      clear_test_resps(test)
      resp_set_a1 <- add_responsibility_set(test, "LAP-elf", "nevis-1")
      resp_set_b1 <- add_responsibility_set(test, "R-deer", "nevis-2")
      resp_a1 <- add_responsibility(test, "nevis-1", resp_set_a1, "hot_chocolate", expec)
      resp_b1 <- add_responsibility(test, "nevis-2", resp_set_b1, "hot_chocolate", expec)
      bes_a1 <- add_burden_estimate_set(test, resp_a1)
      add_burden_estimates(test, pathetic_data(test), bes_a1, resp_a1)

      DBI::dbExecute(test$con, "UPDATE burden_estimate_set
                                   SET responsibility = $1", resp_b1)

      expect_error(stoner::check_ff_consistency(test$con),
        "Inconsistent responsibility/burden_estimate_set linkage")

    }

    test_bes_consistency()

  }



})
