context("unit test: function vgrepl")

## TODO: Rename context
## TODO: Add more tests

test_that("vgrepl assertion works", {
  expect_error(vgrepl(character(), "something"))
  expect_error(vgrepl("something", character()))
  expect_error(vgrepl(character(), character()))
})


test_that("vgrepl works in case of single pattern matching", {
  expect_equal(vgrepl("a", "ab"), grepl("a", "ab"))
  expect_equal(vgrepl("a", "ab"), grepl("a", "ab"))
  expect_equal(vgrepl("a", c("ab", "ac", "bc")), grepl("a", c("ab", "ac", "bc")))
})

test_that("vgrepl behaves the same in case of NA", {
  expect_equal(vgrepl(NA, "ab"), grepl(NA, "ab"))
  expect_equal(vgrepl(NA, NA), grepl(NA, NA))
  expect_equal(vgrepl(NA, c(NA, "a")), grepl(NA, c(NA, "a")))
})

test_that("vgrepl behaves the same in case of additional argument", {
  expect_equal(vgrepl("a", c("ab", "AB"), ignore.case = T), grepl("a", c("ab", "AB"), ignore.case = T))
  expect_equal(vgrepl(NA, c(NA, "a")), grepl(NA, c(NA, "a")))
})