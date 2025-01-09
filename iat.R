# We construct a new data frame from each participant's raw data in order to
# have them in one single format containing all information that we need and not
# more. Then, we calculate the d-scores using the Improved Scoring Algorithm as
# specified above. 
# We also do this on an odd-even split data set to later obtain a split-half
# reliability estimate. 
# We merge all trials and all valid trials together in a single large data frame 
# in case we want to do some further statistics on them. 

iat <- function(df){
  
  df.allvalid <- NULL
  block_names <- c("compatibletest1", "compatibletest2", "incompatibletest1", "incompatibletest2")

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
  df.allvalid <- rbind(df.allvalid, df.current)
  
  return(df.allvalid, df.iat)
}