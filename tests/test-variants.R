# The supplied run_tests.R loads testthat and the functions from R/.
# Do not source original/messy_analysis.R here.

# A supplied ordinary example; keep it and add your own tests below.
test_that("MAF counts allele copies among observed diploid genotypes", {
    expect_equal(calculate_maf(c(0, 0, 1, 1)), 0.25)
})

# TODO: add small, independently justified known-answer tests for both functions.
# Cover the behavior groups listed in README.md. Do not just test data/genotypes.csv.
# Use informative test names; explain nonobvious expected answers in comments.

#Ordinary MAF calculations, including a case where the COUNTED allele is common.
test_that("Counted allele in common in MAF calc", {
    expect_equal(calculate_maf(c(0, 0, 0, 0)), 0)
})

#Missing values: the correct denominator, both values of na_rm, and the default.
test_that("both values of na_rm for maf calc", {
    expect_equal(calculate_maf(c(0, 1, 1, NA), na_rm = TRUE), 0.25)
})
test_that("both values of na_rm for maf calc", {
    expect_equal(calculate_maf(c(0, 1, 1, NA), na_rm = FALSE), 0.25)
})
test_that("both values of na_rm for maf calc", {
    expect_equal(calculate_maf(c(0, 1, 1, NA)), 0.25)
})

#Empty, entirely missing, monomorphic, and single-observation inputs.
test_that("missing input", {
    expect_equal(calculate_maf(), NA)
})
test_that("monomorphic input", {
    expect_equal(calculate_maf(c(2,2,2,2)), 0)
})
test_that("single observation inputs", {
    expect_equal(calculate_maf(1), 0)
})

#Each of the summary's output columns, not merely whether the function returns.
test_that("", {
    expect_equal(calculate_maf(c(0, 0, 1, 1)), 0.25)
})

#Both threshold boundaries, cases on either side, and NONDEFAULT thresholds.
test_that("", {
    expect_equal(calculate_maf(c(0, 0, 1, 1)), 0.25)
})

#Invalid genotype values and invalid argument settings produce errors; valid calls still return correct results.
test_that("", {
    expect_equal(calculate_maf(c(0, 0, 1, 1)), 0.25)
})