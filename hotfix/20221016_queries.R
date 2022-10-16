require(mongolite)
require(progress)
require(glue)

db <- "main"
collection <- "articles"
con <- mongolite::mongo(collection = collection, db = db, verbose = FALSE)

kill <- function(pub, regex, con) {
    con$remove(query = glue::glue('{"pub": "</pub/>", "link": { "$regex": "</regex/>"}}', pub = pub, regex = regex, .open = "</", .close = "/>"))
    testthat::expect_equal(nrow(con$find(query = glue::glue('{"pub": "</pub/>", "link": { "$regex": "</regex/>"}}', pub = pub, regex = regex, .open = "</", .close = "/>"))), 0)
    return(invisible(TRUE))
}


##con$remove(query = '{"pub": "SZ", "link": { "$regex": "[a-z]\\/\\/"}}')
## kill

kill("SZ", "liveticker.sueddeutsche.de", con)
kill("SZ", "newsline2.swmh.de", con)
kill("Zeit", "shop.zeit.de", con)
kill("Zeit", "spiele.zeit.de", con)
kill("Tagesspiegel", "nl.tagesspiegel.de", con)
kill("Freie Welt", "[#?]", con)
kill("Focus", "[#?]", con)


clean <- function(pub, con) {
    print(pub)
    doubles <- con$find(query = paste0('{"link": { "$regex": "[#?]"}, "pub": "', pub,'"}'))
    if (nrow(doubles) == 0) {
        return(FALSE)
    }
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
    }
    for (url in sample(newurls, min(length(newurls), 10000))) {
        testthat::expect_equal(nrow(con$find(query = paste0('{"link": "', url, '"}'))), 1)
    }
}

pubs <- c("Stern", "Merkur", "HNA", "Stuttgarter Zeitung", "Berliner Zeitung", "Bild", "Spiegel", "nordbayern.de", "Tagesspiegel", "Klasse gegen Klasse", "SZ", "Focus", "DieUnbestechlichen", "Hamburger MoPo", "Heute")

purrr::walk(pubs, clean, con = con)
