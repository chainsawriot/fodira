## test_that("test_insert", {
##     skip_on_ci()
##     tryCatch({
##         con <- mongolite::mongo(collection = "articles", db = "main", verbose = FALSE)
##     }, error = function(e) {
##         skip("No MongoDB")
##     })
##     mongolite::mongo("testthat", "main")$drop()
##     testdata <- tibble::tibble(pub = "a", link = "a", pubdate = NA)
##     expect_error({res <- insert_doc_link(testdata, db = "main", collection = "testthat")}, NA) # no error
##     expect_true(nrow(res) == 1)
##     expect_error({res2 <- insert_doc_link(testdata, db = "main", collection = "testthat")}, NA) # no error
##     expect_true(nrow(res2) == 0)
##     ## destroy the collection
##     mongolite::mongo("testthat", "main")$drop()
## })

## test_that("push_html", {
##     skip_on_ci()
##     tryCatch({
##         con <- mongolite::mongo(collection = "articles", db = "main", verbose = FALSE)
##     }, error = function(e) {
##         skip("No MongoDB")
##     })
##     mongolite::mongo("testthat", "main")$drop()
##     mongolite::mongo("htmltt", "main")$drop()
##     testdata <- tibble::tibble(pub = c("a", "a"), link = c("a", "b"), pubdate = NA)
##     expect_error({res <- insert_doc_link(testdata, db = "main", collection = "testthat")}, NA) # no error
##     expect_true(nrow(res) == 2)
##     output <- tibble::tibble(url = c("a", "b"), fname = c("newlinks.RDS", "wwwwwwwwwwwwwwwwww.html"))
##     ## file exists
##     z <- push_html(url = "a", fname = "newlinks.RDS", output_dir = "../testdata/", db = "main", collection = "testthat", prefix = "htmltt", delete = FALSE)
##     expect_false(is.na(z))
##     ## file not exists
##     z <- push_html(url = "b", fname = "wwwwwwwwwwwwwwwwww.html", output_dir = "../testdata/", db = "main", collection = "testthat", prefix = "htmltt", delete = FALSE)
##     expect_true(is.na(z))
##     fs <- mongolite::gridfs(db = "main", prefix = "htmltt")
##     tmp <- tempfile()
##     out <- fs$read("newlinks.RDS", file(tmp), progress = FALSE)
##     x <- readRDS(tmp)
##     y <- readRDS("../testdata/newlinks.RDS")
##     expect_error(fs$read("newlinks.RDS", progress = FALSE), NA)
##     expect_equal(x, y)
##     expect_error(fs$read("wwwwwwwwwwwwwwwwww.html"))
##     mongolite::mongo("testthat", "main")$drop()
##     mongolite::mongo("htmltt", "main")$drop()    
## })

## test_that("push_html_scrape", {
##     skip_on_ci()
##     tryCatch({
##         con <- mongolite::mongo(collection = "articles", db = "main", verbose = FALSE)
##     }, error = function(e) {
##         skip("No MongoDB")
##     })
##     mongolite::mongo("testthat", "main")$drop()
##     mongolite::mongo("htmltt", "main")$drop()
##     testdata <- tibble::tibble(pub = c("a", "a"), link = c("a", "b"), pubdate = NA)
##     expect_error({res <- insert_doc_link(testdata, db = "main", collection = "testthat")}, NA) # no error
##     expect_true(nrow(res) == 2)
##     output <- tibble::tibble(url = c("a", "b"), fname = c("newlinks.RDS", "wwwwwwwwwwwwwwwwww.html"))
##     z <- push_html_scrape(output, output_dir = "../testdata/", db = "main", collection = "testthat", prefix = "htmltt", delete = FALSE)
##     expect_equal(length(z), 2)
##     expect_true(is.na(z[2]))
##     con <- mongolite::mongo(collection = "testthat", db = "main", verbose = FALSE)
##     expect_true(con$find('{"link": "a"}')$htmlfile == "newlinks.RDS")
##     expect_false(con$find('{"link": "a"}')$htmlfile == "wwwwwwwwwwwwwwwwww.html") ## file doesn't exist
##     tmp <- tempfile()
##     fs <- mongolite::gridfs(db = "main", prefix = "htmltt")
##     out <- fs$read("newlinks.RDS", file(tmp), progress = FALSE)
##     x <- readRDS(tmp)
##     y <- readRDS("../testdata/newlinks.RDS")
##     expect_error(fs$read("newlinks.RDS", progress = FALSE), NA)
##     expect_equal(x, y)
##     expect_error(fs$read("wwwwwwwwwwwwwwwwww.html"))
##     mongolite::mongo("testthat", "main")$drop()
##     mongolite::mongo("htmltt", "main")$drop()
## })
