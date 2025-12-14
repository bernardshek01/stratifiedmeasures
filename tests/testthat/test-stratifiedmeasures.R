# Check 1: Stratifiedmeasures Returns the Expected Structure

test_that("stratifiedmeasures_structure", {
  # Create Simulation Data
  set.seed(2025)
  exposure   <- rbinom(100, 1, 0.4)
  outcome    <- rbinom(100, 1, 0.3)
  confounder <- sample(1:3, 100, replace = TRUE)
  simulation_data <- stratified_or(exposure, outcome, confounder)
  # Check for Return Structure
  expect_type(simulation_data, "list")
  expect_named(simulation_data, c("Crude", "Stratified"))
  expect_named(simulation_data$Crude, c("table", "Odds_Ratio"))
  expect_true(is.matrix(simulation_data$Crude$table))
  expect_true(is.numeric(simulation_data$Crude$Odds_Ratio))
  expect_type(simulation_data$Stratified, "list")
})
