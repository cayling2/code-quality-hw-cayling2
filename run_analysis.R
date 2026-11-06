# Run from the repository root.
# TODO: move and simplify the DRIVER portion of original/messy_analysis.R here.
# Read data/genotypes.csv with colClasses = "numeric", call your functions for each column, and write ONE
# combined results/summary.csv after all variant summaries are calculated.
# Put the thresholds in named settings and pass them as function arguments.
# Source R/variant_functions.R to load your function definitions.

#stop("TODO: implement the analysis driver")
genotypes_df <- read.csv("data/genotypes.csv", colClasses = "numeric")
new_df <- data.frame("variant_id" = NA, "n_called" = NA, "missing_rate" = NA, "maf" = NA, "keep" = NA)
col_names <- data.frame(colnames(genotypes_df))
for(i in 1:ncol(genotypes_df))
    genotypes <- genotypes_df[,i]
    #sum var returns a df
    sum_geno <- summarize_variant(genotypes)
    #get the col names
    variant_id <- col_names[i,]
    #n_called, missing_rate, maf, keep
    n_called <- sum_geno[1,]
    missing_rate <- sum_geno[2,]
    maf <- sum_geno[3,]
    keep <- sum_geno[4,]
    new_df <- rbind(new_df, c(variant_id, n_called, missing_rate, maf, keep))
#remove NA row
sum_df <- new_df[-1,]     
#write the sum_df to results/summary.csv
write.csv(sum_df, "./results/summary.csv", col.names = TRUE)