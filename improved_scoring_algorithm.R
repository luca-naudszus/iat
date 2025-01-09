#' improved_scoring_algorithm
#'
#' This function implements the Improved Scoring Algorithm (Greenwald et al., 2003)
#'
#' @param df A data frame containing valid trials for one participant. Must include:
#'   - `subject`: Alphanumeric column, participant ID, non-empty. 
#'   - `blockcode`: Factor column with levels "compatibletest1", "compatibletest2", "incompatibletest1", "incompatibletest2"
#'   - `latency`: Character column, non-empty strings.
#'   - `correct`: Boolean column, indicates whether response was correct. 
#' @return a numeric value, the d-score for this participant
# ---------------------------------------------------------

improved_scoring_algorithm <- function(df){
  # (a) initialize vector with block means
  blockmean <- data.frame(
    blockcode = block_names,
    mean_latency = sapply(block_names, function(block) {
      mean(df$latency[df$blockcode == block & df$correct == TRUE])
    })
  )
  
  # (b) compute pooled SD for practice and test blocks
  SD_prac <- sd(df$latency[df$blockcode %in% c(
    "compatibletest1", "incompatibletest1")])
  SD_test <- sd(df$latency[df$blockcode %in% c(
    "compatibletest2", "incompatibletest2")])
  
  # (c) replace each error latency with block mean + 600 ms
  df <- df %>%
    left_join(blockmean, by = "blockcode") %>%
    mutate(latency = case_when(
      correct == 0 ~ mean_latency + 600,
      TRUE ~ latency
    )) %>%
    select(-mean_latency)
  
  # (d) average all resulting latencies for each block
  df_avg <- df %>%
    group_by(blockcode) %>%
    summarize(mean_latency = mean(latency, na.rm = TRUE))
  
  # (e) compute difference score, depending on block order
  d1 <- df_avg$mean_latency[
    df_avg$blockcode == "incompatibletest1"] - df_avg$mean_latency[
      df_avg$blockcode == "compatibletest1"]
  d2 <- df_avg$mean_latency[
    df_avg$blockcode == "incompatibletest2"] - df_avg$mean_latency[
      df_avg$blockcode == "compatibletest2"]
  dscore <- mean(d1/SD_test, d2/SD_prac)
  
  # (f) return dscore
  return(dscore)
}