### Gene Expression Standardization Pipeline
### Author: Vidur Saigal
### Created on: 2024-12-09
### Description: This pipeline standardizes gene expression values by merging gene expression and 
### location datasets, calculating mean and standard deviation, and outputting the normalized results.

## BLOCK ZERO
## Load necessary libraries
install.packages('tidyverse', repos='http://cran.us.r-project.org')
library(tidyverse)

## BLOCK 0.5
## Obtain file paths for input files from the command line
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2) {
  stop("Please provide two input files: gene expression file and gene location file.")
}
gene_expr_file <- args[1]  # First argument: Gene expression file
gene_loc_file <- args[2]   # Second argument: Gene location file

## BLOCK ONE
## Read input files and merge them based on 'geneid', then reorganize columns
GExpr <- read.table(file = gene_expr_file, sep = ",", header = TRUE)
Loc <- read.table(file = gene_loc_file, sep = ",", header = TRUE)
z <- left_join(GExpr, Loc, by = "geneid") %>%
      relocate(chr, pos, .after = geneid)

## BLOCK TWO
## Calculate average and standard deviation for each gene across expression data
x <- z %>%
  rowwise(geneid) %>%
  mutate(ave = mean(c_across(starts_with("GTEX")), na.rm = TRUE)) %>%
  mutate(sd = sd(c_across(starts_with("GTEX")), na.rm = TRUE)) %>%
  relocate(ave, sd, .after = pos)

## BLOCK THREE
## Standardize gene expression values for each gene
for (i in seq_along(rownames(x))) {
  for (j in 6:length(x[i, ])) {
    this_ave <- x[i, ]$ave
    this_sd <- x[i, ]$sd
    x[i, j] <- (x[i, j] - this_ave) / this_sd
  }
}

## BLOCK FOUR
## Recalculate average and standard deviation for standardized values
x <- x %>%
  rowwise(geneid) %>%
  mutate(ave_std = mean(c_across(starts_with("GTEX")), na.rm = TRUE)) %>%
  mutate(sd_std = sd(c_across(starts_with("GTEX")), na.rm = TRUE)) %>%
  relocate(ave_std, sd_std, .after = sd)

## BLOCK FIVE
## Write the output to a file with a unique name based on the input gene expression file
outfile <- paste0(gene_expr_file, ".std")
write.table(
  x,
  file = outfile,
  sep = ",",
  row.names = FALSE,
  col.names = TRUE,
  quote = FALSE
)