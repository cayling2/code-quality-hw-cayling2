# Refactor the calculations in original/messy_analysis.R into these interfaces.
# Preserve the required behavior described in README.md.
# This file should define functions only: no reads, writes, or analysis run.

calculate_maf <- function(genotypes, na_rm = TRUE) {
    # TODO: calculate MAF for one numeric genotype vector.
    #stop("TODO: implement calculate_maf()")
    #p = sum of genotype values / (2 * number of observed individuals)
    #MAF = min(p, 1 - p)
    #c(0, 0, 1, 1)
    p = sum(genotypes)/(2*length(genotypes))
    MAF = min(p, 1 - p)
    #Empty or entirely missing input: return numeric NA, not 0, NaN, or Inf.
    #All 0 or all 2: return 0. A single heterozygote has MAF 0.5.
    #Reject nonnumeric input and any observed value other than 0, 1, or 2.
    #na_rm must be exactly one nonmissing logical value; reject invalid settings.
    #Validate genotype values even if na_rm = FALSE and missing values are present.
}

summarize_variant <- function(genotypes, min_maf = 0.25,
                              max_missing = 0.25) {
    # TODO: return the specified one-row summary, calling calculate_maf().
    stop("TODO: implement summarize_variant()")
}
