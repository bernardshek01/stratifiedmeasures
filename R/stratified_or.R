#' @title Crude and Stratified Odds Ratio (OR) Estimation with 95\% Confidence Interval
#' @description Computes the crude odds ratio (OR) and stratum-specific odds ratios by a specified confounder using raw exposure and outcome vectors. This function automates the construction of two-way contingency tables with totals and provides odds ratio estimates with 95\% confidence intervals.
#' @param exposure A numeric or factor vector. The exposure may contain multiple levels. The lowest level is treated as the reference group.
#' @param outcome A numeric or factor vector. The outcome can only contain two levels. The lowest level is treated as the reference group.
#' @param confounder A vector representing a potential confounder used for stratification. Each unique non-missing value defines a stratum.
#' @return
#' A list containing two components:
#'  \describe{
#'  \item{Crude}{A list containing the crude contingency table and the crude odds ratio with its 95\% confidence interval.}
#'  \item{Stratified}{A named list of stratum-specific results. For each stratum, the output includes a contingency table and the corresponding odds ratio with its 95\% confidence interval.}
#'  }
#' @details Crude and stratum-specific two-way frequency tables with totals are presented.
#' Odds ratios are estimated using the standard maximum likelihood estimator based on 2*2 contingency tables.
#' Confidence intervals are computed using the mid-p exact method as the default computation method implemented by [epitools::oddsratio()]
#' Missing values from exposure, outcome and confounder variables are excluded from the analysis. Counts of missing values are returned.
#' If any cell in a contingency table contains less than 5 counts, a warning message is returned as implemented by [epitools::oddsratio()]
#' @examples
#' ## Generate simulation data
#' set.seed(123)
#' exposure <- rbinom(100, 1, 0.4)
#' outcome <- rbinom(100, 1, 0.3)
#' confounder <- sample(1:3, 100, replace = TRUE)
#' ## Run stratified odds ratio analysis
#' stratified_or(exposure, outcome, confounder)
#' @importFrom stats addmargins
#' @export

stratified_or <- function(exposure, outcome, confounder) {

  # DATA CHECKS
  # Length Check
  if(length(unique(c(length(exposure), length(outcome), length(confounder)))) != 1) {stop("Exposure, Outcome, and Confounder must all be the same length.")}
  # Numeric Check
  # Outcome Check
  n_levels_outcomes <- unique(outcome[!is.na(outcome)])
  if(length(n_levels_outcomes) != 2) {stop("Outcome must have exactly two levels.")}
  if(!(is.numeric(outcome) | is.factor(outcome))) {stop("Outcome must be a factor or numeric.")}
  else if(is.factor(outcome)) {message("Outcome is a factor. Using factor level order as the numeric coding: ", paste(levels(outcome), collapse = " < "))}
  n_missing_outcomes <- sum(is.na(outcome))
  if(n_missing_outcomes > 0) {message("Outcome contains ", n_missing_outcomes, " missing observation(s) excluded from calculation.")}
  # Exposure Check
  n_levels_exposure <- unique(exposure[!is.na(exposure)])
  if(length(n_levels_exposure) > 5) {warning("Exposure may be continuous. Please double check if the exposure is categorical or ordinal.")}
  else if(length(n_levels_exposure) < 2) {warning("Exposure contains no variation. Odds Ratio may be infinite or undefined.")}
  if(!(is.numeric(exposure) | is.factor(exposure))) {stop("Exposure must be a factor or numeric.")}
  else if (is.factor(exposure)) {message("Exposure is a factor. Using factor level order as the numeric coding: ", paste(levels(exposure), collapse = " < "))}
  n_miss_exposure <- sum(is.na(exposure))
  if(n_miss_exposure > 0) {message("Exposure contains ", n_miss_exposure, " missing observation(s) excluded from calculation.")}
  # Confounder Check
  n_missing_confounder <- sum(is.na(confounder))
  if(n_missing_confounder > 0) {message("Note: Confounder has ", n_missing_confounder, " missing value(s). NA will be treated as an unprinted stratum.")}
  n_levels_confounder <- unique(confounder[!is.na(confounder)])
  if(length(n_levels_confounder) > 5) {warning("Confounder may be continuous. Please double check the data type.")}
  else if(length(n_levels_confounder) < 2) {warning("Confounder contains no variation. Only crude Odds Ratio is presented.")}

  # CRUDE 2x2 TABLE AND ODDS RATIO (OR)
  crude_dataframe <- data.frame(exposure = exposure, outcome = outcome, confounder = confounder)
  crude_or <- epitools::oddsratio(crude_dataframe$exposure, crude_dataframe$outcome)
  crude_2x2_table <- crude_or[["data"]]
  crude_or_values <- crude_or[["measure"]]

  # STRATIFIED 2x2 TABLE AND ODDS RATIO (OR)
  dataframe_list <- split(crude_dataframe, crude_dataframe$confounder)
  stratified_results <- list()
  for(i in seq_along(dataframe_list)){
    stratified_dataframe <- dataframe_list[[i]]
    stratified_or <- epitools::oddsratio(stratified_dataframe$exposure, stratified_dataframe$outcome)
    stratified_results[[i]] <- list(table = stratified_or[["data"]], Odds_Ratio = stratified_or[["measure"]][-1, ])
  }
  names(stratified_results) <- names(dataframe_list)

  # RETURN FINAL RESULTS
  result <- list(Crude = list(table = crude_2x2_table, Odds_Ratio = crude_or_values[-1, ]), Stratified = stratified_results)
  return(result)

}
