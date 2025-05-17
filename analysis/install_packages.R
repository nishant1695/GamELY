
# List of required packages
required_packages <- c(
  "irrCAC",
  "dplyr",
  "ggplot2",
  "tidyr"
)

# Install missing packages
installed <- rownames(installed.packages())
for (pkg in required_packages) {
  if (!(pkg %in% installed)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

# Confirm installed
cat("All required packages are installed.\n")
