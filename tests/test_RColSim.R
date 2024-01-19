library(testthat)


scr <<- "tests_hist" 
run_type<<- "supply_and_demand"



# Run RColSim for the test scenarios
# Run the RColSim main code
source("RColSim_main.R")
output_test <- read.table("tests/output_tests/supply_and_demand/Historical_baseline/dams_out.txt", header = T)

expected_output <- read.table("tests/output_tests/Validation/dams_out.txt", header = T)

# Compare the results with the default outputs --> MICAA
test_that("MICAA functions works correctly", {
  test_station <- "MICAA"
  results <- output_test[,test_station]
  results_expected <- expected_output[, test_station]
  expect_equal(results,results_expected, label = "Check model outflow from Dalles")
})

# Compare the results with the default outputs --> ARROW
test_that("ARROW functions works correctly", {
  test_station <- "ARROW"
  results <- output_test[,test_station]
  results_expected <- expected_output[, test_station]
  expect_equal(results,results_expected, label = "Check model outflow from ARROW")
})


# Compare the results with the default outputs --> LIBBY
test_that("LIBBY functions works correctly", {
  test_station <- "LIBBY"
  results <- output_test[,test_station]
  results_expected <- expected_output[, test_station]
  expect_equal(results,results_expected, label = "Check model outflow from LIBBY")
})


# Compare the results with the default outputs --> GCOUL
test_that("GCOUL functions works correctly", {
  test_station <- "GCOUL"
  results <- output_test[,test_station]
  results_expected <- expected_output[, test_station]
  expect_equal(results,results_expected, label = "Check model outflow from GCOUL")
})


# Compare the results with the default outputs --> ICEHA
test_that("ICEHA functions works correctly", {
  test_station <- "ICEHA"
  results <- output_test[,test_station]
  results_expected <- expected_output[, test_station]
  expect_equal(results,results_expected, label = "Check model outflow from ICEHA")
})

# Compare the results with the default outputs --> DUNCA
test_that("DUNCA functions works correctly", {
  test_station <- "DUNCA"
  results <- output_test[,test_station]
  results_expected <- expected_output[, test_station]
  expect_equal(results,results_expected, label = "Check model outflow from DUNCA")
})

# Compare the results with the default outputs --> DALLE
test_that("DALLE functions works correctly", {
  test_station <- "DALLE"
  results <- output_test[,test_station]
  
  results_expected <- expected_output[, test_station]
  expect_equal(results,results_expected,  label = "Check model outflow from Dalles")
})
