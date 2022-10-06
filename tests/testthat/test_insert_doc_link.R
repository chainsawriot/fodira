test_that("test_insert", {
    skip_on_ci()
    testdata <- tibble::tibble(pub = "a", link = "a", pubdate = NA)
    expect_error({res <- insert_doc_link(testdata, db = "main", collection = "testthat")}, NA) # no error
    expect_true(nrow(res) == 1)
    expect_error({res2 <- insert_doc_link(testdata, db = "main", collection = "testthat")}, NA) # no error
    expect_true(nrow(res2) == 0)
    ## destroy the collection
    mongolite::mongo("testthat", "main")$drop()
})
