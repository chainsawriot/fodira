#!/usr/bin/env Rscript

library(fodira)
args <- commandArgs(trailingOnly=TRUE)

.get_urls <- function(con, safe) {
    if (safe) {
        con$aggregate('[ {"$match": {"htmlfile": "", "pub" : { "$in": ["Bild", "Tagesschau", "Heute", "T-Online"]}}}, { "$sample": { "size": 300} }]')$link
    } else {
        con$aggregate('[ {"$match": {"htmlfile": "", "pub": { "$nin": ["Zeit"]}}}, { "$sample": { "size": 300} }]')$link
    }
}

con <- mongolite::mongo("articles", db = "main")
saveRDS(.get_urls(con, safe = TRUE), args[1])
