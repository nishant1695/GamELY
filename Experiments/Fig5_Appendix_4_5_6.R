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

# Compute agreement for anomaly vs random (equal size)
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
  
  total_rows <- nrow(R1)
  set.seed(42)  # for reproducibility
  random_indices <- sample(1:total_rows, length(anomaly_indices))  # sample from full data
  
  results_df <- data.frame()
  
  for (q in ques) {
    # Anomaly sample
    ratings_anom <- data.frame(
      R1 = as.factor(R1[[q]][anomaly_indices]),
      R2 = as.factor(R2[[q]][anomaly_indices]),
      R3 = as.factor(R3[[q]][anomaly_indices])
    )
    ratings_anom <- ratings_anom[apply(ratings_anom, 1, function(x) all(x != -1)), ]
    
    # Random sample
    ratings_rand <- data.frame(
      R1 = as.factor(R1[[q]][random_indices]),
      R2 = as.factor(R2[[q]][random_indices]),
      R3 = as.factor(R3[[q]][random_indices])
    )
    ratings_rand <- ratings_rand[apply(ratings_rand, 1, function(x) all(x != -1)), ]
    
    agreement_anom <- if (nrow(ratings_anom) >= 2) irrCAC::gwet.ac1.raw(ratings_anom)$est$coeff.val else NA
    agreement_rand <- if (nrow(ratings_rand) >= 2) irrCAC::gwet.ac1.raw(ratings_rand)$est$coeff.val else NA
    
    results_df <- rbind(results_df, data.frame(
      Dataset = name,
      Question = q,
      Agreement_Random = agreement_rand,
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

# ==== Map long question names ====

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

question_map <- lapply(question_map, function(x) gsub(" ", ".", x))
reverse_question_map <- setNames(names(question_map), unname(question_map))


# ==== Create and Save Plot function ====

create_and_save_plot <- function(results_df, filename) {
  agg_df <- results_df %>%
    pivot_longer(cols = c(Agreement_Random, Agreement_Anomalies), names_to = "Type", values_to = "Value") %>%
    mutate(Type = recode(Type,
                         Agreement_Random = "Random",
                         Agreement_Anomalies = "Anomalies")) %>%
    group_by(Question, Type) %>%
    summarise(
      Mean = mean(Value, na.rm = TRUE),
      SD = sd(Value, na.rm = TRUE),
      .groups = 'drop'
    )
  
  agg_df$Question <- reverse_question_map[as.character(agg_df$Question)]
  
  p <- ggplot(agg_df, aes(x = Question, y = Mean, fill = Type)) +
    geom_col(position = position_dodge(0.8), width = 0.7) +
    geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                  position = position_dodge(0.8), width = 0.25, color = "gray40") +
    scale_fill_manual(values = c("Random" = "#2b8cbe", "Anomalies" = "#f03b20")) +
    labs(
      title = "",
      x = "",
      y = "IRR Score",
      fill = ""
    ) +
    theme_minimal(base_size = 16) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, size = 22),
      axis.text.y = element_text(size = 22),
      plot.title = element_text(hjust = 0.5, size = 22, face = "bold"),
      axis.title = element_text(size = 22, face = "bold"),
      legend.position = "top",
      legend.direction = "horizontal",
      legend.justification = "center",
      legend.box = "horizontal",
      legend.text = element_text(size = 22),
      legend.title = element_text(size = 22)
    )
  
  ggsave(filename, plot = p, width = 15, height = 8, dpi = 600)
}

# ==== Plot Combined Results (Fig5) ====

agg_df_all <- all_results %>%
  pivot_longer(cols = c(Agreement_Random, Agreement_Anomalies), names_to = "Type", values_to = "Value") %>%
  mutate(Type = recode(Type,
                       Agreement_Random = "Random",
                       Agreement_Anomalies = "Anomalies")) %>%
  group_by(Question, Type) %>%
  summarise(
    Mean = mean(Value, na.rm = TRUE),
    SD = sd(Value, na.rm = TRUE),
    .groups = 'drop'
  )

agg_df_all$Question <- reverse_question_map[as.character(agg_df_all$Question)]

bar_plot <- ggplot(agg_df_all, aes(x = Question, y = Mean, fill = Type)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                position = position_dodge(0.8), width = 0.25, color = "gray40") +
  scale_fill_manual(values = c("Random" = "#2b8cbe", "Anomalies" = "#f03b20")) +
  labs(
    title = "",
    x = "",
    y = "IRR Score",
    fill = ""
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 22),
    axis.text.y = element_text(size = 22),
    plot.title = element_text(hjust = 0.5, size = 22, face = "bold"),
    axis.title = element_text(size = 22, face = "bold"),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.justification = "center",
    legend.box = "horizontal",
    legend.text = element_text(size = 22),
    legend.title = element_text(size = 22)
  )

ggsave("./Fig5.pdf", plot = bar_plot, width = 15, height = 8, dpi = 600)

# ==== Plot and Save Separate Results for Appendix 4, 5, 6 ====

create_and_save_plot(results_aha, "./Appendix_4.pdf")
create_and_save_plot(results_cc, "./Appendix_5.pdf")
create_and_save_plot(results_diabetes, "./Appendix_6.pdf")
