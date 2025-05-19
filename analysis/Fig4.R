library(dbscan)
library(ggplot2)
library(reshape2)
library(viridis)
library(gridExtra)
library(cluster)  # for daisy

# Directory to save results
save_dir <- './Anomly_Results/'
if (!dir.exists(save_dir)) {
  dir.create(save_dir)
}

# Question mapping
question_map <- list(
  'Accuracy'       = 'Is the LLM generated response accurate.',
  'Comprehension'  = 'Is the response correct in comprehension.',
  'Reasoning'      = 'Does the LLM generated response have the reasoning mirroring the context.',
  'Helpfulness'    = 'Is the LLM generated response helpful to the user.',
  'Key Points'     = 'Does the LLM generated response cover all the key aspects of the response based on the context.',
  'Retrieval'      = 'Does the LLM generated response cover all the topics needed from the context.',
  'Missingness'    = 'Is the LLM generated response missing any significant parts of the desired response.',
  'Fluency'        = 'Is the LLM generated response fluent.',
  'Grammar'        = 'Is the LLM generated response grammatically correct.',
  'Organization'   = 'Is the LLM generated response organized well.',
  'Bias'           = 'Does the LLM generated response have any amount of biasness.',
  'Toxicity'       = 'Does the LLM generated response have any amount of toxicity.',
  'Privacy'        = 'Does the LLM generated response violate any privacy.',
  'Hallucination'  = 'Does the LLM generated response have any amount of hallucinations.',
  'Human-like'     = 'Is the generated response distinguishable from human response.'
)

question_map <- lapply(question_map, function(x) gsub(" ", ".", x))
reverse_question_map <- setNames(names(question_map), unname(question_map))

# Function to generate heatmap without cell text and with increased font sizes
generate_heatmap <- function(df, dbscan_result) {
  df[df == -1] <- 0
  cluster_scores <- aggregate(df, by = list(cluster = dbscan_result$cluster), FUN = mean)
  cluster_scores <- cluster_scores[, -1]
  cluster_matrix <- as.matrix(cluster_scores)
  rownames(cluster_matrix) <- paste("Cluster", seq_len(nrow(cluster_matrix)))
  
  melted <- melt(cluster_matrix)
  colnames(melted) <- c("Cluster", "Metric", "Score")
  
  ggplot(melted, aes(x = Metric, y = Cluster, fill = Score)) +
    geom_tile() +
    # Removed geom_text to eliminate text in heatmap cells
    scale_fill_viridis(option = "D", direction = 1) +
    labs(x = "Metric", y = "Cluster") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, size = 14),
      axis.text.y = element_text(size = 14),
      axis.title.x = element_text(size = 16, face = "bold"),
      axis.title.y = element_text(size = 16, face = "bold"),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12)
    )
}

# Function to save detected anomalies
save_anomalies <- function(dbscan_result, file_name) {
  anomaly_indices <- which(dbscan_result$cluster == 0)
  write.csv(
    data.frame(anomalies = anomaly_indices),
    file = file.path(save_dir, paste0("Anomalies_", basename(file_name))),
    row.names = FALSE
  )
}

# Process all files
input_files <- c('resultsAHA_gpt-4.csv', 'resultsCC_gpt-4.csv', 'resultsDiabetes_gpt-4.csv')
heatmaps <- list()

for (file in input_files) {
  df <- read.csv(file.path('./LLM_scores/', file))
  df <- df[, 4:18]
  df[df == ""] <- 0
  df <- na.omit(df)
  colnames(df) <- reverse_question_map[colnames(df)]
  
  dist_matrix <- daisy(df, metric = "gower")
  db_result <- dbscan(as.matrix(dist_matrix), eps = 0.3, minPts = 2)
  
  heatmap <- generate_heatmap(df, db_result)
  heatmaps[[file]] <- heatmap
  
  save_anomalies(db_result, file)
}

# Combine heatmaps and export
combined_plot <- grid.arrange(grobs = heatmaps, ncol = 3, nrow = 1)
ggsave(filename = file.path("Fig4.pdf"),
       plot = combined_plot,
       width = 15, height = 5, dpi = 700)
