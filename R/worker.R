#' Pack the job from `work_dl`
#'
#' Pack the job from `work_dl` as a single tar.gz
#' 
#' @param output_file archive
#' @param rds rds file
#' @param output_dir html directory
#' @param delete whether to delete `rds` and `output_dir` after packing
#' @return `output_file`, invisibily.
#' @author Chung-hong Chan
#' @export
#' @importFrom utils tar
pack_work <- function(output_file = "job.tar.gz", rds, output_dir, delete = FALSE) {
    tar(output_file , files = c(rds, output_dir), compression = "gzip")
    if (delete) {
        unlink(rds, recursive = TRUE)
        unlink(output_dir, recursive = TRUE)
    }
    return(invisible(output_file))
}

#' Get links (urls) from the DB
#'
#' This function gets links (urls) from the DB where the field `htmlfile` is empty, i.e. not yet scraped.
#'
#' @param fname file name of the RDS file. If null, the function returns a vector of links
#' @param size number of links to fetch
#' @param safe whether to only get links from "safe" sources
#' @param db MongoDB db
#' @param collection Mongodb collection
#' @param unsafe_pubs a vector of publications to avoid
#' @return either a vector of links (if `fname` is null) or the `fname` invisibly
#' @author Chung-hong Chan
#' @export
get_links <- function(fname = NULL, size = 300, safe = FALSE, collection = "articles", db = "main", unsafe_pubs = c("Zeit", "Saarbrücker Zeitung", "RT deutsch")) {
    con <- mongolite::mongo(collection = collection, db = db)
    if (safe) {
        links <- con$aggregate(paste0('[ {"$match": {"htmlfile": "", "pub" : { "$in": ["Bild", "Tagesschau", "Heute", "Freitag", "T-Online"]}}}, { "$sample": { "size": ', size, '} }]'))$link
    } else {
        links <- con$aggregate(paste0('[ {"$match": {"htmlfile": "", "pub": { "$nin": [', paste0(purrr::map_chr(unsafe_pubs, ~paste0('"', ., '"')), collapse = ", ") , ']}}}, { "$sample": { "size": ', size, '} }]'))$link
    }
    if (is.null(fname)) {
        return(links)
    } else {
        saveRDS(links, fname)
        return(invisible(fname))
    }
}
