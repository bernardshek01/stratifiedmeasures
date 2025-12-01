library(epitools)

E <- c(1,1,1,1, 0,0,0,0,   1,1,1,1, 0,0,0,0)  # 8 for C=0, 8 for C=1
D <- c(1,0,1,0, 1,0,1,0,   1,1,0,0, 1,0,1,0)
C <- c(0,0,0,0, 0,0,0,0,   1,1,1,1, 1,1,1,1)

set.seed(2025)

generate_clean_data <- function(n = 100) {
  repeat {
    # Confounder
    C <- rbinom(n, 1, 0.5)

    # Exposure depends on C (mild confounding)
    E <- rbinom(n, 1, ifelse(C == 1, 0.65, 0.45))

    # Outcome probability avoids extremes (0.15–0.65)
    prob_D <- 0.15 + 0.20*E + 0.20*C
    D <- rbinom(n, 1, prob_D)

    # Build stratified table
    tab3d <- table(E, D, C)

    # Check for zero cells in ANY stratum
    if (all(tab3d > 0)) {
      return(data.frame(E, D, C))
    }
  }
}

df <- generate_clean_data(100)
df




tab3d <- table(E, D, C)
tab3d

# Odds ratios for each level of C
or_by_stratum <- apply(tab3d, 3, function(m) {
  meas <- oddsratio(m)$measure
  # row 2 = comparison group (1 vs 0)
  meas[2, c("estimate", "lower", "upper")]
})

or_by_stratum




# Risk ratios for each level of C
rr_by_stratum <- apply(tab3d, 3, function(m) riskratio(m)$measure)
rr_by_stratum
