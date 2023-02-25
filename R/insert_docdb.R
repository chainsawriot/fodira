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

#' Insert a single url and html file to MongoDB
#' This function takes the output from [scrape()] and inserts it to the MongoDB (default to main.articles and main.html). Concretely, it updates the `htmlfile` field of the matching records and pushes the HTML files in `output_dir` into MongoDB's GridFS.
#' @param url a url
#' @param fname file name of the HTML file
#' @param output_dir a directory to hold HTML files
#' @param db MongoDB db
#' @param collection MongoDB collection
#' @param prefix S3 path
#' @param delete whether to delete the HTML files afterwards.
#' @return The file name uploaded to GridFS, return NA if `fname` doesn't exist
#' @author Chung-hong Chan
#' @export
push_html <- function(url, fname, output_dir = Sys.getenv("ARTICLE_DIR"), db = "main", collection = "articles", prefix = "S3://fodira-html/", delete = TRUE) {
    con <- mongolite::mongo(collection = collection, db = db)
    htmlpath <- file.path(output_dir, fname)
    if (file.exists(htmlpath)) {
        query <- paste0('{"link": "', url,'"}')
        update <- paste0('{"$set": { "htmlfile": "', fname,'"}}')
        con$update(query, update)
	## upload to S3
        res <- system2("s3cmd", args = c("put", htmlpath, prefix))
	if (res != 0) {
            stop("S3 Upload failed.")
        }
        if (delete) {
            unlink(htmlpath)
        }
        return(htmlpath)
    }
    return(NA) ## file not found
}

#' Insert output and html files from scrape() to MongoDB
#' This function takes the output from [scrape()] and inserts it to the MongoDB (default to main.articles and main.html). Concretely, it updates the `htmlfile` field of the matching records and pushes the HTML files in `output_dir` into MongoDB's GridFS.
#' @param output data.frame from [scrape()]
#' @return a vector of file names uploaded to GridFS (invisibly)
#' @author Chung-hong Chan
#' @export
#' @inheritParams push_html
push_html_scrape <- function(output, output_dir = Sys.getenv("ARTICLE_DIR"), db = "main", collection = "articles", prefix = "S3://fodira-html/", delete = TRUE) {
    uploaded_files <- c()
    if (nrow(output) > 0) {
        con <- mongolite::mongo(collection = collection, db = db)
        fs <- mongolite::gridfs(db = db, prefix = prefix)
        for (i in seq_len(nrow(output))) {
            htmlpath <- push_html(url = output[i, "url"], fname = output[i, "fname"], output_dir = output_dir,
                                  db = db, collection = collection, prefix = prefix, delete = delete)
            uploaded_files <- c(uploaded_files, htmlpath)
        }
    }
    return(invisible(uploaded_files))
}

#' Insert output from [pack_work()] to MongoDB
#' This function takes the output from [pack_work()] and inserts it to the MongoDB (default to main.articles and main.html). Concretely, it updates the `htmlfile` field of the matching records and pushes the HTML files into MongoDB's GridFS.
#' @param job_fname path of the job (a tar.gz file) from [pack_work()]
#' @param delete whether to delete `job_fname` after the insertion
#' @author Chung-hong Chan
#' @export
#' @inheritParams push_html
push_html_job <- function(job_fname, db = "main", collection = "articles", prefix = "S3://fodira-html/", delete = TRUE) {
    random_temp_path <- file.path(tempdir(), generate_hash(ending = ""))
    suppressMessages(utils::untar(job_fname, exdir = random_temp_path))
    all_files <- list.files(random_temp_path, full.names = TRUE, recursive = TRUE)
    output <- readRDS(grep("output\\.RDS$", all_files, value = TRUE))
    output_html <- dirname(sample(grep("html", all_files, value = TRUE), 1))
    res <- push_html_scrape(output = output, output_dir = output_html, db = db, collection = collection, prefix = prefix, delete = delete)
    if (delete) {
        unlink(job_fname)
    }
    return(invisible(res))
}
