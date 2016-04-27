context("vgrepl")

## TODO: Rename context
## TODO: Add more tests

test_that("vgrepl assertion works", {
  expect_error(vgrepl(character(), "something"))
  expect_error(vgrepl("something", character()))
  expect_error(vgrepl(character(), character()))
})


