install.packages("gplots", repos='http://cran.us.r-project.org')
library(gplots)

# Function to process a single plate
process_plate <- function(platefile, platename, threshold = -4) {
  # Step 1: Load the plate data
  myplate <- read.csv(platefile, header = TRUE, sep = ",")
  
  # Step 2: Calculate mean and standard deviation for max controls (columns 1 and 23)
  max_controls <- c(myplate[, 1], myplate[, 23])
  mean_max <- mean(max_controls, na.rm = TRUE)
  sd_max <- sd(max_controls, na.rm = TRUE)
  
  # Step 3: Calculate mean and standard deviation for min controls (columns 2 and 24)
  min_controls <- c(myplate[, 2], myplate[, 24])
  mean_min <- mean(min_controls, na.rm = TRUE)
  sd_min <- sd(min_controls, na.rm = TRUE)
  
  # Step 4: Calculate the Z-prime factor
  Z_prime <- 1 - (3 * (sd_max + sd_min)) / abs(mean_max - mean_min)
  
  # Step 5: Normalize the data (Z-scores)
  myplate_zscores <- as.data.frame(lapply(myplate, function(column) (column - mean_max) / sd_max))
  
  # Step 6: Exclude controls and count cells below the threshold
  data_without_controls <- myplate_zscores[, -c(1, 2, 23, 24)]  # Remove control columns
  num_cells_below_threshold <- sum(data_without_controls < threshold, na.rm = TRUE)
  
  # Return results as a list
  return(list(
    platename = platename,
    Z_prime = Z_prime,
    num_cells_below_threshold = num_cells_below_threshold
  ))
}

# Set the working directory to "HomeWork III"
setwd("/Users/vidursaigal/Documents/Documents - Vidur’s MacBook Pro/Github Repos/CIS5350-Homework/Homework III/PlateData")

# Define plates to process (plate2.csv to plate7.csv)
plates <- list(
  list(file = "plate2.csv", name = "plate2"),
  list(file = "plate3.csv", name = "plate3"),
  list(file = "plate4.csv", name = "plate4"),
  list(file = "plate5.csv", name = "plate5"),
  list(file = "plate6.csv", name = "plate6"),
  list(file = "plate7.csv", name = "plate7")
)

# Initialize a results list
results <- list()

# Process each plate
for (plate in plates) {
  result <- process_plate(plate$file, plate$name)
  results <- append(results, list(result))
}

# Print results for each plate
for (result in results) {
  cat("Plate:", result$platename, "\n")
  cat("Z-prime Factor:", result$Z_prime, "\n")
  cat("Cells Below Threshold:", result$num_cells_below_threshold, "\n")
  cat("-------------------------\n")
}

