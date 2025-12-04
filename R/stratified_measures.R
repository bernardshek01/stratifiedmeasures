#' @title X
#' @description X
#' @param X
#' @param X
#' @return X
#' @examples X
#' @export

library(epitools)

stratified_measures <- function(exposure, outcome, confounder) {
  # DATA CHECKS
  # Length Check
  if(length(unique(c(length(exposure), length(outcome), length(confounder)))) != 1) {stop("Exposure, Outcome, and Confounder must all be the same length.")}
  # Numeric Check
  if(!is.numeric(exposure)) stop("Exposure must be numeric (Coded 0/1).")
  if(!is.numeric(outcome)) stop("Outcome must be numeric (Coded 0/1).")
  if(!is.numeric(confounder)) stop("Confounder must be numeric.")
  if(!all(exposure %in% c(0,1))) {stop("Exposure must be coded only as 0 and 1.")}
  if(!all(outcome %in% c(0,1))) {stop("Outcome must be coded only as 0 and 1.")}
  if(length(unique(exposure)) != 2) {warning("Exposure contains no variation (All 0 or All 1). Odds Ratio or Relative Risk may be infinite or undefined.")}
  if(length(unique(outcome)) != 2) {warning("Outcome contains no variation (All 0 or All 1). Odds Ratio or Relative Risk may be infinite or undefined.")}
  # BUILD 3D TABLE
  exposure_factor <- factor(exposure, levels = c(0,1))
  outcome_factor <- factor(outcome, levels = c(0,1))
  confounder_factor <- factor(confounder)
  table_3d <- table(exposure = exposure_factor, outcome = outcome_factor, confounder = confounder_factor)
  # STRATIFIED MEASURES
  strata_levels <- dimnames(table_3d)[["confounder"]]
  tables_by_stratum <- lapply(seq_along(strata_levels), function(k) {table_3d[, , k]})
  names(tables_by_stratum) <- strata_levels
  # STRATIFIED ODDS RATIO
  OR_list <- list()
  for (k in seq_along(strata_levels)) {
    stratified_table <- table_3d[, , k]
    OR_statistic_value_with_CI <- oddsratio(stratified_table)$measure
    OR_list[[k]] <- data.frame(Stratum = strata_levels[k], Measure = "Odds Ratio (OR)", Reference = 0, Exposure = 1, Estimate = OR_statistic_value_with_CI[2, 1], Lower_95_CI = OR_statistic_value_with_CI[2, 2], Upper_95_CI = OR_statistic_value_with_CI[2, 3])
  }
  OR_with_all_stratums <- do.call(rbind, OR_list)
  # STRATIFIED RELATIVE RISK
  RR_list <- list()
  for (k in seq_along(strata_levels)) {
    stratified_table <- table_3d[, , k]
    RR_statistic_value_with_CI <- riskratio(stratified_table)$measure
    RR_list[[k]] <- data.frame(Stratum = strata_levels[k], Measure = "Relative Risk (RR)", Reference = 0, Exposure = 1, Estimate = RR_statistic_value_with_CI[2, 1], Lower_95_CI = RR_statistic_value_with_CI[2, 2], Upper_95_CI = RR_statistic_value_with_CI[2, 3])
  }
  RR_with_all_stratums <- do.call(rbind, RR_list)
  # RETURNING EVERYTHING CALCULATED
  result <- list(tables_by_stratum = tables_by_stratum, Odds_Ratio = OR_with_all_stratums, Relative_Risk = RR_with_all_stratums)
  return(result)
}

# TESTING DATA

set.seed(2025)

generate_test_data <- function(n = 500) {

  # Confounder with 4 levels
  C <- sample(0:3, n, replace = TRUE, prob = c(0.25, 0.25, 0.25, 0.25))

  # Exposure depends somewhat on C
  # (higher C → slightly higher chance of exposure)
  prob_E <- plogis(-0.5 + 0.4*C)   # logistic function
  E <- rbinom(n, 1, prob_E)

  # Outcome depends on both E and C
  # (E has positive effect, C has small effect)
  prob_D <- plogis(-1 + 1.0*E + 0.3*C)
  D <- rbinom(n, 1, prob_D)

  data.frame(E = E, D = D, C = C)
}

df_large <- generate_test_data(500)
df_large

res <- stratified_measures(df_large$E, df_large$D, df_large$C)
res
