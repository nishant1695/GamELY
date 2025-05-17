# ==== Load Libraries ====
library(irrCAC)
library(dplyr)
library(ggplot2)
library(tidyr)

# ==== Helper Functions ====

# Transform ratings into 3 levels and handle NAs
transform_ratings <- function(data, ques) {
  data[ques] <- lapply(data[ques], function(col) {
    col <- ifelse(col %in% c(1, 2), 2, col)  # Disagreement
    col <- ifelse(col %in% c(4, 5), 4, col)  # Agreement
    col[is.na(col)] <- -1                   # Missing
    return(col)
  })
  return(data)
}

# Compute agreement for full vs anomalies
process_dataset_anomalies <- function(name, r1_path, r2_path, r3_path, gpt_path, anomaly_path) {
  R1 <- read.csv(r1_path)
  R2 <- read.csv(r2_path)
  R3 <- read.csv(r3_path)
  GPT <- read.csv(gpt_path)
  anomaly_indices <- read.csv(anomaly_path)$anomalies
  
  ques <- names(GPT)[4:18]
  R1 <- transform_ratings(R1, ques)
  R2 <- transform_ratings(R2, ques)
  R3 <- transform_ratings(R3, ques)
  
  results_df <- data.frame()
  
  for (q in ques) {
    ratings_full <- data.frame(
      R1 = as.factor(R1[[q]]),
      R2 = as.factor(R2[[q]]),
      R3 = as.factor(R3[[q]])
    )
    ratings_full <- ratings_full[apply(ratings_full, 1, function(x) all(x != -1)), ]
    
    ratings_anom <- data.frame(
      R1 = as.factor(R1[[q]][anomaly_indices]),
      R2 = as.factor(R2[[q]][anomaly_indices]),
      R3 = as.factor(R3[[q]][anomaly_indices])
    )
    ratings_anom <- ratings_anom[apply(ratings_anom, 1, function(x) all(x != -1)), ]
    
    agreement_full <- if (nrow(ratings_full) >= 2) irrCAC::gwet.ac1.raw(ratings_full)$est$coeff.val else NA
    agreement_anom <- if (nrow(ratings_anom) >= 2) irrCAC::gwet.ac1.raw(ratings_anom)$est$coeff.val else NA
    
    results_df <- rbind(results_df, data.frame(
      Dataset = name,
      Question = q,
      Agreement_Full = agreement_full,
      Agreement_Anomalies = agreement_anom,
      stringsAsFactors = FALSE
    ))
  }
  
  return(results_df)
}

# ==== Process Datasets ====
results_aha <- process_dataset_anomalies(
  "AHA",
  "./human_scores/R1_AHA scores.csv",
  "./human_scores/R2_AHA scores.csv",
  "./human_scores/R3_AHA scores.csv",
  "./LLM_scores/resultsAHA_gpt-4.csv",
  "./Anomly_Results/Anomalies_resultsAHA_gpt-4.csv"
)

results_cc <- process_dataset_anomalies(
  "CC",
  "./human_scores/R1_CC scores.csv",
  "./human_scores/R2_CC scores.csv",
  "./human_scores/R3_CC scores.csv",
  "./LLM_scores/resultsCC_gpt-4.csv",
  "./Anomly_Results/Anomalies_resultsCC_gpt-4.csv"
)

results_diabetes <- process_dataset_anomalies(
  "Diabetes",
  "./human_scores/R1_Diabetes scores.csv",
  "./human_scores/R2_Diabetes scores.csv",
  "./human_scores/R3_Diabetes scores.csv",
  "./LLM_scores/resultsDiabetes_gpt-4.csv",
  "./Anomly_Results/Anomalies_resultsDiabetes_gpt-4.csv"
)

# Combine all results
all_results <- bind_rows(results_aha, results_cc, results_diabetes)

# ==== Grouped Bar Plot (Fig5) ====

# Prepare aggregated data
agg_df <- all_results %>%
  pivot_longer(cols = starts_with("Agreement"), names_to = "Type", values_to = "Value") %>%
  group_by(Question, Type) %>%
  summarise(
    Mean = mean(Value, na.rm = TRUE),
    SD = sd(Value, na.rm = TRUE),
    .groups = 'drop'
  )

# Map long question names
question_map <- list(
  'Accuracy' = 'Is the LLM generated response accurate.',
  'Comprehension' = 'Is the response correct in comprehension.',
  'Reasoning' = 'Does the LLM generated response have the reasoning mirroring the context.',
  'Helpfulness' = 'Is the LLM generated response helpful to the user.',
  'Key Points' = 'Does the LLM generated response cover all the key aspects of the response based on the context.',
  'Retrieval' = 'Does the LLM generated response cover all the topics needed from the context.',
  'Missingness' = 'Is the LLM generated response missing any significant parts of the desired response.',
  'Fluency' = 'Is the LLM generated response fluent.',
  'Grammar' = 'Is the LLM generated response grammatically correct.',
  'Organization' = 'Is the LLM generated response organized well.',
  'Bias' = 'Does the LLM generated response have any amount of biasness.',
  'Toxicity' = 'Does the LLM generated response have any amount of toxicity.',
  'Privacy' = 'Does the LLM generated response violate any privacy.',
  'Hallucination' = 'Does the LLM generated response have any amount of hallucinations.',
  'Human-like' = 'Is the generated response distinguishable from human response.'
)

# Clean and reverse map
question_map <- lapply(question_map, function(x) gsub(" ", ".", x))
reverse_question_map <- setNames(names(question_map), unname(question_map))
agg_df$Question <- reverse_question_map[as.character(agg_df$Question)]

# Plot grouped bar chart
bar_plot <- ggplot(agg_df, aes(x = Question, y = Mean, fill = Type)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                position = position_dodge(0.8), width = 0.25, color = "gray40") +
  geom_text(aes(label = round(Mean, 2)),
            position = position_dodge(0.8),
            vjust = -0.7, size = 3, fontface = "bold") +
  scale_fill_manual(values = c("#2b8cbe", "#f03b20")) +
  labs(
    title = "",
    x = "",
    y = "Mean Agreement",
    fill = ""
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 12, face = "bold")
  )

# Save the grouped bar plot
ggsave("./Fig5.pdf", plot = bar_plot, width = 15, height = 8, dpi = 600)
