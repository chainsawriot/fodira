.detect_atom <- function(x) {
    any(grepl("^entry", colnames(x)))
}

## entry_last_updated -> "item_pub_date"
## entry_title -> "item_title"
## entry_url -> "item_link"
## entry_summary -> "item_description"
.convert_atom <- function(x) {
    ## colnames(x)[]
    ## dplyr::rename(x, .data$item_title = `entry_title`, .data$item_link = `entry_url`, .data$item_pub_date = `entry_last_updated`, .data$item_description = `entry_summary`)
    return(x)
}

.silent_parse <- function(url, id = NA, debug = NA) {
    if (!is.na(debug)) {
        message(url)
    }
    suppressMessages(res <- tidyRSS::tidyfeed(url))
    res$id <- id
    res$url <- url
    if (.detect_atom(res)) {
        res <- .convert_atom(res)
    }
    if (!"item_pub_date" %in% colnames(res)) {
        res$item_pub_date <- NA
    }
    return(res)
}
