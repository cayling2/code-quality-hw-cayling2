# Refactor the calculations in original/messy_analysis.R into these interfaces.
# Preserve the required behavior described in README.md.
# This file should define functions only: no reads, writes, or analysis run.

calculate_maf <- function(genotypes, na_rm = TRUE) {
    # TODO: calculate MAF for one numeric genotype vector.
    #Empty or entirely missing input: return numeric NA, not 0, NaN, or Inf.
    if(missing(genotypes))
        return(as.numeric(NA))
    #if any geno missing with na_rm F return NA
    if(na_rm == FALSE)
        geno2 <- na.omit(genotypes)
        if(length(geno2) != length(genotypes))
            return(NA)
    #Reject nonnumeric input and any observed value other than 0, 1, or 2.
    #is.numeric
    if(!is.numeric(genotypes))
        stop("That is a nonnumeric value")
    geno2 <- na.omit(genotypes)
    if(sum(geno2) != 0)
        if(sum(geno2) == (length(geno2)))
            if(sum(geno2) == (length(geno2)*2))
                stop("These are not within the values accepted")
    #All 0 or all 2: return 0. A single heterozygote has MAF 0.5.
    if(sum(geno2) == 0)
        return(0)
    if(sum(geno2) == (length(geno2)*2))
        return(0)
    #na_rm must be exactly one nonmissing logical value; reject invalid settings.
    if(missing(na_rm))
        stop("Invalid setting for na_rm")
    if(na_rm != TRUE)
        if(na_rm != FALSE)
            stop("Invalid setting for na_rm")
    #Validate genotype values even if na_rm = FALSE and missing values are present.
    #stop("TODO: implement calculate_maf()")
    #p = sum of genotype values / (2 * number of observed individuals)
    #MAF = min(p, 1 - p)
    #c(0, 0, 1, 1)
    p = sum(geno2)/(2*length(geno2))
    MAF = min(p, 1 - p)
    #Empty or entirely missing input: return numeric NA, not 0, NaN, or Inf.
    #All 0 or all 2: return 0. A single heterozygote has MAF 0.5.
    #Reject nonnumeric input and any observed value other than 0, 1, or 2.
    #na_rm must be exactly one nonmissing logical value; reject invalid settings.
    #Validate genotype values even if na_rm = FALSE and missing values are present.
    return(MAF)
}

summarize_variant <- function(genotypes, min_maf = 0.25,
                              max_missing = 0.25) {
    # TODO: return the specified one-row summary, calling calculate_maf().
    #stop("TODO: implement summarize_variant()")
    #Both thresholds are INCLUSIVE. They are arguments, not hard-coded rules. 
    #For empty input, return n_called = 0, missing_rate = NA, maf = NA, keep = FALSE.
    if(missing(genotypes))
        return(data.frame(c(0, NA, NA, FALSE)))
    #For a nonempty entirely missing input, the missing rate is 1 and keep is FALSE.
    #if(is.na(genotypes))
    #    missing_rate <- 1
    #    keep <- FALSE
    #    total_length <- length(genotypes)
    #    num_missing <- 0
    #    for(i in 1:total_length)
    #        if(is.na(i))
    #            num_missing <- num_missing + 1
    #    n_called <- total_length - num_missing
    #    maf <- calculate_maf(genotypes, na_rm = TRUE)
    #    to_return <- data.frame(c(n_called, missing_rate, maf, keep))
    #    return(to_return)
    #min_maf must be one finite numeric value between 0 and 0.5 inclusive.
    if(min_maf > 0.5)
        if(min_maf < 0)
            stop("min_maf must be one finite numeric value between 0 and 0.5 inclusive")
    #max_missing must be one finite numeric value between 0 and 1 inclusive. 
    if(max_missing > 1)
        if(max_missing < 0)
            stop("max_missing must be one finite numeric value between 0 and 1 inclusive.")
    #Reject invalid inputs/settings with informative errors. 
    #Exact error wording is not prescribed. Call calculate_maf() here; do not duplicate its calculation.
    #Return a ONE-ROW data frame with exactly these columns in this order:
    #n_called, missing_rate, maf, keep
    geno2 <- na.omit(genotypes)
    total_length <- length(geno2)
    num_missing <- 0
    for(i in 1:total_length)
        if(is.na(i))
            num_missing <- num_missing + 1
    n_called <- total_length - num_missing
    missing_rate <- num_missing/total_length
    maf <- calculate_maf(genotypes, na_rm = TRUE)
    keep <- FALSE
    if (maf >= min_maf) 
        if(missing_rate <= max_missing)
            keep <- TRUE
    to_return <- data.frame(c(n_called, missing_rate, maf, keep))
    return(to_return)
}
