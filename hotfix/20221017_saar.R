require(mongolite)
require(progress)
require(glue)

db <- "main"
collection <- "articles"
con <- mongolite::mongo(collection = collection, db = db, verbose = TRUE)

con$update('{"pub":"Saarbr�cker Zeitung"}', '{"$set":{"pub": "Saarbrücker Zeitung"}}', multiple = TRUE)

testthat::expect_equal(nrow(con$find('{"pub":"Saarbr�cker Zeitung"}')), 0)
