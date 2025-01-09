# Analysis of IAT results using the Improved Scoring Algorithm (Greenwald et al., 2003)
This repository implements the Improved Scoring Algorithm for evaluation of Implicit Association Test results. Split-half reliability assessment is also contained. 

## Required dependencies
* tidyverse

---

## Functions  

1. `iat.R`  
- **Purpose**: Computes d-scores for all participants in the dataset.  
- **Input**: A data frame containing trial-level data from all participants, where each row represents a single trial.  
  - **Required columns**:  
    - `subject`: Participant IDs.  
    - `blockcode`: Specifies the block each trial belongs to, expected to be one of four types:  
      - `compatibletest1`  
      - `compatibletest2`  
      - `incompatibletest1`  
      - `incompatibletest2`  
    - `latency`: Reaction times in milliseconds.  
    - `correct`: Boolean indicating whether the response was correct.  
- **Output**: A table containing:  
  - Overall d-scores for each participant.  
  - D-scores from an odd-even trial split for further analyses.  

2. `improved_scoring_algorithm.R`  
- **Purpose**: A helper function required by `iat.R` to implement the d-score computation based on an improved scoring algorithm.  

3. `shr.R`  
- **Purpose**: Computes the split-half reliability of the d-scores generated by `iat.R`, adjusted using the Spearman-Brown formula.  
- **Input**: The output table from `iat.R`.  
- **Output**: A reliability coefficient reflecting the internal consistency of the d-scores.  

---

## Overview  
This repository facilitates efficient computation of IAT d-scores and reliability analysis for robust psychometric evaluation.  

## Citation
When using this work, please cite Naudszus, Altmann, & Roth (2024). Relationships between self-esteem-related dream content and explicit and implicit self-esteem. Acta Psychologica, 251, 104582. https://doi.org/10.1016/j.actpsy.2024.104582


