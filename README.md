# iat

## Analysis of IAT results using the Improved Scoring Algorithm (Greenwald et al., 2003)
This repository implements the Improved Scoring Algorithm for evaluation of Implicit Association Test results. 

# Required dependencies
*tidyverse

# Usage
It contains three functions: `iat.R`, `improved_scoring_algorithm.R`, `shr.R`.
`iat.R` takes a data frame with all trials from all participants as an input. Each trial is in one row. 
The function expects at least four columns: subject, blockcode, latency, and correct. 
*`subject` contains the participants' ids. 
*`blockcode` gives an information on which block the trial belongs to. The trials are expected to belong to one out of four blocks: compatibletest1, compatibletest2, incompatibletest1, incompatibletest2. 
*`latency` contains the reaction latency in ms. 
*`correct` is a boolean describing whether the response was correct. 
The output is a table containing d-scores for all participants (as well as d-scores from an odd-even split used below). 

`improved_scoring_algorithm.R` is a helper function. 

`shr.R` takes the output of `iat.R` as an input and returns the split-half reliability, corrected with the Spearman-Brown-Formula. 


