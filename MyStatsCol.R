####################################################################
##  MyStatsCol
MyStatsCol <- function(x,i){
  mi <- round(min(x[,i], na.rm = TRUE),4)
  q10 <- round(quantile(x[,i],probs = 0.10,  na.rm=TRUE),4)
  q25 <- round(quantile(x[,i],probs = 0.25,  na.rm=TRUE),4)
  md <- round(median(x[,i], na.rm = TRUE),4)
  mn <- round(mean(x[,i], na.rm = TRUE),4)
  st <- round(sd(x[,i], na.rm = TRUE),4)
  q75 <- round(quantile(x[,i], probs = 0.75, na.rm = TRUE),4)
  q90 <- round(quantile(x[,i], probs = 0.90, na.rm = TRUE),4)
  mx <- round(max(x[,i], na.rm = TRUE))
  ul <- length(unique(x[complete.cases(x[,i]),i]))
  na <- sum(is.na(x[,i]))
  results <- c(mi, q10, q25, md, mn, st, q75, q90, mx, ul, na)
  names(results) <- c("Min.", "Q.10", "Q.25", "Median", "Mean",
                      "Std.Dev.", "Q.75", "Q.90", "Max.",
                      "Unique", "NA's")
  results
}