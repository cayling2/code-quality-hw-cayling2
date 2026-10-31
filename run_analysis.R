# Run from the repository root.
# TODO: move and simplify the DRIVER portion of original/messy_analysis.R here.
# Read data/genotypes.csv with colClasses = "numeric", call your functions for each column, and write ONE
# combined results/summary.csv after all variant summaries are calculated.
# Put the thresholds in named settings and pass them as function arguments.
# Source R/variant_functions.R to load your function definitions.

#stop("TODO: implement the analysis driver")
genotypes_df <- read.csv("data/genotypes.csv", colClasses = "numeric")
new_df <- data.frame(NA)
for(i in ncol(genotypes_df))
    genotypes <- genotypes_df[,i]
    sum_geno <- summarize_variant(genotypes)