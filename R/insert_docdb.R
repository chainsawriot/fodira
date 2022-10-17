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

#' Insert output and html files from scrape() to MongoDB
#' This function takes the output from [scrape()] and inserts it to the MongoDB (default to main.articles and main.html). Concretely, it updates the `htmlfile` field of the matching records and pushes the HTML files in `output_dir` into MongoDB's GridFS.
#' @param output data.frame from [scrape()]
#' @param output_dir a directory to hold HTML files
#' @param db MongoDB db
#' @param collection MongoDB collection
#' @param prefix MongoDB GridFS prefix
#' @param delete whether to delete the HTML files afterwards.
#' @return a vector of file names uploaded to GridFS (invisibly)
#' @author Chung-hong Chan
#' @export
push_html <- function(output, output_dir = Sys.getenv("ARTICLE_DIR"), db = "main", collection = "articles", prefix = "html", delete = TRUE) {
    uploaded_files <- c()
    if (nrow(output) > 0) {
        con <- mongolite::mongo(collection = collection, db = db)
        fs <- mongolite::gridfs(db = db, prefix = prefix)
        for (i in seq_len(nrow(output))) {
            htmlpath <- file.path(output_dir, output$fname[i])
            if (file.exists(htmlpath)) {
                query <- paste0('{"link": "', output[i, "url"],'"}')
                update <- paste0('{"$set": { "htmlfile": "', output[i, "fname"],'"}}')
                con$update(query, update)
                ## push GridFS
                fs$upload(htmlpath)
                uploaded_files <- c(uploaded_files, htmlpath)
                if (delete) {
                    unlink(htmlpath)
                }
            }
        }
    }
    return(invisible(uploaded_files))
}
