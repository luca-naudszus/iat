#' improved_scoring_algorithm
#'
#' This function calculates the split-half reliability, corrected with the Spearman-Brown formula. 
#'
#' @param df.iat A data frame containing d-scores for all participants. Must include:
#'   - `dscore.odd`: Numeric column, d-scores from the odd splits. 
#'   - `dscore.even`: Numeric column, d-scores from the even splits. 
#' @return a numeric value, the split-half reliability in this dataset
# ---------------------------------------------------------

shr <- function(df.iat){
  shr <- cor(df.iat$dscore.odd, df.iat$dscore.even, 
           use = "pairwise.complete.obs")
  rel <- 2*shr/(1+shr)
  return(shr)
}