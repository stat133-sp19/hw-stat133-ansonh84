context("Test main functions")

test_that("test that bin_choose returns an error if k>n, returns only 1 value, and can correctly calculate specified choose values", {
  expect_error(bin_choose(11, 30))
  expect_length(bin_choose(20, 10), 1)
  expect_equal(bin_choose(10,5), 250)
})
test_that("test that bin_probability returns an error if its inputs are negative, returns multiple probabilities if needed, and returns the correct probability value", {
  expect_error(bin_probability(-12, 32, 0.33))
  expect_error(bin_probability(10, -32, 0.43))
  expect_error(bin_probability(10, 31, -0.35))
  expect_length(bin_probability(1:5, 25, 0.22), 6)
  expect_equal(bin_probability(1, 5, 0.5), 0.1476)
})
test_that("test that bin_distribution returns an error if its inputs are negative, returns an object of class data.frame and bindis, and has two columns", {
  expect_error(bin_distribution(-10, 0.25))
  expect_error(bin_distribution(10, -0.35))
  expect_is(bin_distribution(10, 0.45), c("bindis", "data.frame"))
  expect_length(bin_distribution(25, 0.32), 2)
})
test_that("test that bin_cumulative returns an error if its inputs are negative, returns an object of class data.frame and bincum, and has three columns", {
  expect_error(bin_cumulative(-10, 0.45))
  expect_error(bin_cumulative(10, -0.65))
  expect_is(bin_cumulative(10, 0.75), c("bincum", "data.frame"))
  expect_length(bin_cumulative(25, 0.32), 3)
})
