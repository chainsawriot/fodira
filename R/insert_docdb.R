#' Insert links to MongoDB and return new links
#' This function inserts links (from either the RSS aggregator or archive scrappers) into the MongoDB (default to main.articles).
#' @param links, a data.frame / tibble of the docdb format: pub, link, pubdate, title, description
#' @param debug logical, whether debug message should be displayed
#' @param db MongoDB db
#' @param collection Mongodb collection
#' @return A docdb-formated tibble of new links, which are not yet in the collection before the insertion
#' @author Chung-hong Chan
#' @export
insert_doc_link <- function(links, debug = FALSE, db = "main", collection = "articles") {
    newlinks <- tibble::tibble()
    con <- mongolite::mongo(collection = collection, db = db, verbose = debug)
    for (i in seq_len(nrow(links))) {
        existing_link <- con$find(paste0('{"link": "', links[i, "link"], '"}'))
        if (isTRUE(debug)) {
            message(links[i, "link"])
            if (nrow(existing_link) == 0) {
                message("is a new link.")
            }
        }
        if (nrow(existing_link) == 0) {
            to_be_inserted <- as.data.frame(links[i,])
            if (lubridate::tz(to_be_inserted$pubdate) != "UTC") {
                to_be_inserted$pubdate <- lubridate::with_tz(to_be_inserted$pubdate, "UTC")
            }
            con$insert(to_be_inserted)
            newlinks <- dplyr::bind_rows(newlinks, to_be_inserted)
        }
    }
    return(newlinks)
}
