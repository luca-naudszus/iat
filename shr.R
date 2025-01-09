# (3) Split-Half Reliability with Spearman-Brown formula

# ... using odd-even splitting

shr <- function(df.iat){
  shr <- cor(df.iat$dscore.odd, df.iat$dscore.even, 
           use = "pairwise.complete.obs")
  rel <- 2*shr/(1+shr)
  return(shr)
}