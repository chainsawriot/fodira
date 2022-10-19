test_that("test_generate_hash", {
    expect_error(generate_hash(), NA)
    expect_error(generate_hash(ending = ""), NA)
    expect_equal(nchar(generate_hash(ending = "", length = 10)), 10)
    expect_true(grepl("RDS$", generate_hash(ending = "RDS", length = 10)))
    ## seed
    set.seed(123)
    x <- generate_hash()
    set.seed(123)
    y <- generate_hash()
    expect_equal(x, y)
    ## unseed
    x <- generate_hash()
    y <- generate_hash()
    expect_false(x == y)
})
