test_that("test_get_links", {
    skip_on_ci()
    tryCatch({
        con <- mongolite::mongo(collection = "articles", db = "main", verbose = FALSE)
    }, error = function(e) {
        skip("No MongoDB")
    })
    expect_error({x <- get_links(size = 10)}, NA)
    expect_equal(length(x), 10)
    tempfile <- tempfile(fileext = ".RDS")
    expect_error({x <- get_links(fname = tempfile, size = 10)}, NA)
    expect_true(file.exists(tempfile))
    y <- readRDS(tempfile)
    expect_equal(length(y), 10)
    unlink(tempfile)
})
