# Fig3_IRR_Correlation_GwetAC1.R

# Load required libraries
library(irrCAC)
library(dplyr)
library(ggplot2)
library(tidyr)

# ---- Helper Function: Transform categorical ratings into broader bands ----
transform_ratings <- function(data, questions) {
  data[questions] <- lapply(data[questions], function(col) {
    col <- ifelse(col %in% c(1, 2), 2, col)  # Disagreement band
    col <- ifelse(col %in% c(4, 5), 4, col)  #  Agreement band
    col[is.na(col)] <- -1                   # Mark missing ratings
    return(col)
  })
  return(data)
}

# ---- Helper Function: Get majority vote among 3 raters ----
get_majority <- function(r1, r2, r3) {
  apply(cbind(r1, r2, r3), 1, function(x) {
    ux <- unique(x[x != -1])
    if (length(ux) == 0) return(NA)
    tab <- table(factor(x, levels = ux))
    names(tab)[which.max(tab)]
  })
}

# ---- Core Function: Process one dataset ----
process_dataset <- function(dataset_name, r1_path, r2_path, r3_path, llm_path) {
  # Load data
  R1 <- read.csv(r1_path)
  R2 <- read.csv(r2_path)
  R3 <- read.csv(r3_path)
  GPT <- read.csv(llm_path)
  
  # Identify rating questions
  questions <- names(GPT)[4:18]
  
  # Transform ratings
  R1 <- transform_ratings(R1, questions)
  R2 <- transform_ratings(R2, questions)
  R3 <- transform_ratings(R3, questions)
  GPT <- transform_ratings(GPT, questions)
  
  # Initialize result storage
  results_df <- data.frame()
  
  # Evaluate over different random sampling percentages
  for (percent in seq(10, 100, by = 5)) {
    set.seed(999)
    sampled_rows <- sample(1:nrow(GPT), size = floor(percent / 100 * nrow(GPT)), replace = FALSE)
    
    for (q in questions) {
      # Full dataset (excluding missing ratings)
      full_ratings <- data.frame(
        R1 = as.factor(R1[[q]]),
        R2 = as.factor(R2[[q]]),
        R3 = as.factor(R3[[q]])
      )
      full_ratings <- full_ratings[apply(full_ratings, 1, function(x) all(x != -1)), ]
      
      # Random subset
      rand_ratings <- data.frame(
        R1 = as.factor(R1[[q]][sampled_rows]),
        R2 = as.factor(R2[[q]][sampled_rows]),
        R3 = as.factor(R3[[q]][sampled_rows])
      )
      rand_ratings <- rand_ratings[apply(rand_ratings, 1, function(x) all(x != -1)), ]
      
      # Calculate IRR
      irr_full <- if (nrow(full_ratings) >= 2) irrCAC::gwet.ac1.raw(full_ratings)$est$coeff.val else NA
      irr_rand <- if (nrow(rand_ratings) >= 2) irrCAC::gwet.ac1.raw(rand_ratings)$est$coeff.val else NA
      
      # Store results
      results_df <- rbind(results_df, data.frame(
        Dataset = dataset_name,
        Question = q,
        Random_Selection_Percent = percent,
        Agreement_Full = irr_full,
        Agreement_Random = irr_rand,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  return(results_df)
}

# ---- Apply to all datasets ----
results_diabetes <- process_dataset(
  "Diabetes",
  "./human_scores/R1_Diabetes scores.csv",
  "./human_scores/R2_Diabetes scores.csv",
  "./human_scores/R3_Diabetes scores.csv",
  "./LLM_scores/resultsDiabetes_gpt-4.csv"
)

results_cvd <- process_dataset(
  "CVD",
  "./human_scores/R1_AHA scores.csv",
  "./human_scores/R2_AHA scores.csv",
  "./human_scores/R3_AHA scores.csv",
  "./LLM_scores/resultsAHA_gpt-4.csv"
)

results_cc <- process_dataset(
  "CC",
  "./human_scores/R1_CC scores.csv",
  "./human_scores/R2_CC scores.csv",
  "./human_scores/R3_CC scores.csv",
  "./LLM_scores/resultsCC_gpt-4.csv"
)

# ---- Combine all results ----
all_results <- bind_rows(results_diabetes, results_cvd, results_cc)

# ---- Correlation Analysis ----
correlations <- all_results %>%
  group_by(Dataset, Random_Selection_Percent) %>%
  filter(!is.na(Agreement_Full), !is.na(Agreement_Random)) %>%
  summarise(
    Correlation = cor(Agreement_Full, Agreement_Random, method = "pearson"),
    CI_Lower = cor.test(Agreement_Full, Agreement_Random)$conf.int[1],
    CI_Upper = cor.test(Agreement_Full, Agreement_Random)$conf.int[2],
    .groups = 'drop'
  )

# ---- Plot IRR Correlation ----
p <- ggplot(correlations, aes(x = Random_Selection_Percent, y = Correlation)) +
  geom_line(color = "#FFA500", size = 1.2) +
  geom_point(color = "#FFA500", size = 2) +
  geom_ribbon(aes(ymin = CI_Lower, ymax = CI_Upper), fill = "darkgreen", alpha = 0.2) +
  facet_wrap(~ Dataset, nrow = 1) +
  labs(
    x = "Random Selection Percentage",
    y = "IRR Correlation: Full vs Random Subset"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 13, face = "bold", color = "#444444"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# ---- Save figure ----
ggsave("./Fig3.pdf", plot = p, width = 12, height = 4, dpi = 600)
