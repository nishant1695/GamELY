# combine_for_Fig1_Appendix_2_3_4.R

# Load required libraries
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(ggdendro)
library(dendextend)

# Define paths
model_path <- './LLM_scores/'
human_path <- './human_scores/'

# Dataset and model configurations
datasets <- c("AHA", "CC", "Diabetes")
llms <- c("deepseek-chat", "gpt-4", "gpt-4o")
reviewers <- c("R1", "R2", "R3")

# Mapping of short names to question text
question_map <- list(
  'Accuracy' = 'Is the LLM generated response accurate?',
  'Comprehension' = 'Is the response correct in comprehension?',
  'Reasoning' = 'Does the LLM generated response have the reasoning mirroring the context?',
  'Helpfulness' = 'Is the LLM generated response helpful to the user?',
  'Key Points' = 'Does the LLM generated response cover all the key aspects of the response based on the context?',
  'Retrieval' = 'Does the LLM generated response cover all the topics needed from the context?',
  'Missingness' = 'Is the LLM generated response missing any significant parts of the desired response?',
  'Fluency' = 'Is the LLM generated response fluent?',
  'Grammar' = 'Is the LLM generated response grammatically correct?',
  'Organization' = 'Is the LLM generated response organized well?',
  'Bias' = 'Does the LLM generated response have any amount of biasness?',
  'Toxicity' = 'Does the LLM generated response have any amount of toxicity?',
  'Privacy' = 'Does the LLM generated response violate any privacy?',
  'Hallucination' = 'Does the LLM generated response have any amount of hallucinations?',
  'Human-like' = 'Is the generated response distinguishable from human response?'
)

question_texts <- unname(unlist(question_map))

# Function to calculate metrics
calculate_irrelevancy_metrics <- function(dataset, llm) {
  cat("Processing:", dataset, llm, "\n")
  
  gpt_file <- paste0("results", dataset, "_", llm, ".csv")
  gpt_data <- read_csv(file.path(model_path, gpt_file), show_col_types = FALSE)
  gpt_data[gpt_data == ""] <- NA
  
  reviewer_files <- paste0(reviewers, "_", dataset, " scores.csv")
  reviewer_paths <- file.path(human_path, reviewer_files)
  
  reviewer_data <- list()
  for (path in reviewer_paths) {
    if (file.exists(path)) {
      reviewer_data[[path]] <- read_csv(path, show_col_types = FALSE)
    }
  }
  
  ques <- names(gpt_data)[4:18]
  result_table <- data.frame()
  
  for (q in ques) {
    if (!q %in% question_texts) next
    
    reviewer_scores <- lapply(reviewer_data, function(df) df[[q]])
    reviewer_matrix <- do.call(cbind, reviewer_scores)
    
    true_pos <- apply(!is.na(reviewer_matrix), 1, any)
    llm_pred <- gpt_data[[q]]
    llm_pos <- !is.na(llm_pred)
    
    TP <- sum(llm_pos & true_pos, na.rm = TRUE)
    FP <- sum(llm_pos & !true_pos, na.rm = TRUE)
    TN <- sum(!llm_pos & !true_pos, na.rm = TRUE)
    FN <- sum(!llm_pos & true_pos, na.rm = TRUE)
    
    accuracy <- (TP + TN) / (TP + FP + TN + FN)
    precision <- ifelse((TP + FP) > 0, TP / (TP + FP), NA)
    recall <- ifelse((TP + FN) > 0, TP / (TP + FN), NA)
    f1_score <- ifelse((precision + recall) > 0, 2 * (precision * recall) / (precision + recall), NA)
    
    result_table <- bind_rows(result_table, data.frame(
      Dataset = dataset,
      LLM = llm,
      Question = q,
      TP = TP,
      FP = FP,
      TN = TN,
      FN = FN,
      Accuracy = accuracy,
      Precision = precision,
      Recall = recall,
      F1_Score = f1_score
    ))
  }
  
  return(result_table)
}

# Aggregate results
all_results <- data.frame()
for (dataset in datasets) {
  for (llm in llms) {
    res <- calculate_irrelevancy_metrics(dataset, llm)
    all_results <- bind_rows(all_results, res)
  }
}

# Reverse map long question text to short form
reverse_question_map <- setNames(names(question_map), question_texts)
all_results <- all_results %>%
  mutate(Matrics = sapply(Question, function(q) {
    if (q %in% names(reverse_question_map)) {
      return(reverse_question_map[[q]])
    } else {
      return("Unknown Question")
    }
  }))

# Pivot long for plotting
all_results_long <- all_results %>%
  pivot_longer(cols = c(Accuracy, Recall, F1_Score, Precision),
               names_to = "Metric", values_to = "Value") %>%
  mutate(Dataset = case_when(
    grepl("AHA", Dataset) ~ "CVD",
    grepl("CC", Dataset) ~ "CC",
    grepl("Diabetes", Dataset) ~ "Diabetes",
    TRUE ~ "Other"
  ))

# Plotting function
generate_heatmap_plot <- function(metric_name, output_filename) {
  data_metric <- all_results_long %>% filter(Metric == metric_name)
  
  question_order <- hclust(dist(as.matrix(table(data_metric$Matrics, data_metric$LLM))), method = "ward.D2")
  ordered_questions <- labels(question_order)[order.dendrogram(as.dendrogram(question_order))]
  
  p <- ggplot(data_metric, aes(x = Matrics, y = LLM, fill = Value)) +
    geom_tile(color = "white") +
    facet_wrap(~ Dataset, scales = "free", ncol = 1) +
    scale_fill_gradient(low = "red", high = "white", na.value = "grey90") +
    theme_minimal(base_size = 14) +
    labs(
      x = "Evaluation Aspect",
      y = "Model",
      fill = metric_name
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(size = 26)
    ) +
    scale_x_discrete(limits = ordered_questions) +
    geom_text(aes(label = round(Value, 2)), color = "black", size = 4)
  
  ggsave(output_filename, plot = p, dpi = 700, height = 10, width = 10)
}

# Generate all figures
generate_heatmap_plot("Recall", "./Fig1.pdf")
generate_heatmap_plot("Accuracy", "./Appendix_Fig2.pdf")
generate_heatmap_plot("F1_Score", "./Appendix_Fig3.pdf")
