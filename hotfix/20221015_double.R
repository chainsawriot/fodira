require(mongolite)
require(progress)

##2207385

db <- "main"
collection <- "articles"
con <- mongolite::mongo(collection = collection, db = db)

## kill these SZ links
## Ref: https://github.com/chainsawriot/fodira/issues/13#issuecomment-1279728608
con$remove(query = '{"pub": "SZ", "link": { "$regex": "[a-z]\\/\\/"}}')

## sanity check
testthat::expect_equal(nrow(con$find(query = '{"pub": "SZ", "link": { "$regex": "[a-z]\\/\\/"}}')), 0)

doubles <- con$find(query = '{"link": { "$regex": "[a-z]\\/\\/"}}')

pb <- progress_bar$new(total = nrow(doubles))

newurls <- c()

for (i in seq_len(nrow(doubles))) {
    pb$tick()
    oldurl <- doubles$link[i]
    newurl <- fodira::modify_url(oldurl)
    newurls <- c(newurls, newurl)
    ## remove any record with newurl
    con$remove(query = paste0('{"link": "', newurl, '"}'))
    con$update(query = paste0('{"link": "', oldurl, '"}'), update = paste0('{"$set": { "link": "', newurl,'"}}'))
    con$find(query = paste0('{"link": "', newurl, '"}'))
}

## sanity check
## actually without sample() also works; but to save time.
for (url in sample(newurls, 1000)) {
    testthat::expect_equal(1, nrow(con$find(query = paste0('{"link": "', url, '"}'))))
}
