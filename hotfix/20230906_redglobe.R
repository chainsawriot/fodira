require(mongolite)
require(progress)
require(glue)

db <- "main"
collection <- "articles"
con <- mongolite::mongo(collection = collection, db = db, verbose = TRUE)

## pub <- "Redglobe"
## res1 <- mongocon$find(paste0('{"pub": "', pub,'"}'), '{"link": true, "htmlfile": true, "pubdate": true}')

## head(res1)

## res2 <- mongocon$find(sprintf('{"pub": "%s", "link": "%s"}', "Red Globe", res1$link[2]), '{"link": true}')

con$update('{"pub":"Redglobe"}', '{"$set":{"pub": "Red Globe"}}', multiple = TRUE)

## duplicate urls

## db.articles.aggregate([{$match: {pub: "Red Globe"}}, {$group: {_id:"$link", count: {$sum: 1}}}, {$sort: {"count":1}}])
