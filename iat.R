#' iat
#'
#' This function processes iat test results and returns d-scores.
#'
#' @param df A data frame containing all trials for all participants. Must include:
#'   - `subject`: Alphanumeric column, participant ID, non-empty. 
#'   - `blockcode`: Factor column with levels "compatibletest1", "compatibletest2", "incompatibletest1", "incompatibletest2"
#'   - `latency`: Character column, non-empty strings.
#'   - `correct`: Boolean column, indicates whether response was correct. 
#' @return A data frame containing iat values for all participants. Includes an odd-even split for calculation of split-half reliability.
# ---------------------------------------------------------

source('improved_scoring_algorithm.R')
iat <- function(df){
  
  df.valid <- NULL
  block_names <- c("compatibletest1", "compatibletest2", 
                   "incompatibletest1", "incompatibletest2")

  df.iat <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(df.iat) <- c("id", "dscore", "dscore.odd", "dscore.even")

  for (current_id in unique(df.all$subject)){
    df.current <- df.all[df.all$subject == current_id,]
  
    ### eliminate rows that do not belong to a trial
    df.current <- df.current[df.current$blockcode %in% block_names,]
  
    # Clean data
    
    ### eliminate subject in case more than 10% of latencies are below 300 ms
    ### eliminate subject in case more than 10% of latencies are above 10.000 ms
    ### eliminate subject in case more than 25% of trials are missing
    too_fast <- df.current[df.current$latency < 300,]
    too_slow <- df.current[df.current$latency > 10000,]
    missing <- df.current[is.na(df.current$latency),]
    if (nrow(too_fast) / nrow(df.current) > .1 | 
        nrow(missing) / nrow(df.current) > .25) {
      new_row = cbind(current_id, NA, NA, NA)
      colnames(new_row) <- colnames(df.iat)
      df.iat <- rbind(df.iat, new_row)
      if (nrow(too_fast) / nrow(df.current) > .1) {
        sprintf("Excluding %d: too many latencies below 300 ms", current_id)
      } 
      if (nrow(missing) / nrow(df.current) > .25) {
        sprintf("Excluding %d: too many missings", current_id)
      }
      next
    }
  
    ### eliminate trials with latencies above 10.000 ms and below 400 ms
    df.current <- df.current[df.current$latency < 10000,]
    df.current <- df.current[df.current$latency > 400,]
    df.current <- df.current[!is.na(df.current$latency),]
  
    # split data for split-half reliability calculations
    # (odd-even split)
    df.odd <- df.current[seq(from = 1, to = nrow(df.current), by = 2),]
    df.even <- df.current[seq(from = 2, to = nrow(df.current), by = 2),]
  
    # use above specified function on all three data frames
    dscore <- improved_scoring_algorithm(df.current)
    dscore.odd <- improved_scoring_algorithm(df.odd)
    dscore.even <- improved_scoring_algorithm(df.even)
  
    # save d-score
    new_row = cbind(current_id, dscore, dscore.odd, dscore.even)
    colnames(new_row) <- colnames(df.iat)
    df.iat <- rbind(df.iat, new_row)
  
  # bind data from all valid trials in one data frame
  df.valid <- rbind(df.valid, df.current)
  
  return(df.valid, df.iat)
  }
}