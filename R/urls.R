#' @export
modify_url <- function(url, rm_query = TRUE, replace_domains = NULL) {
    parsed_url <- httr::parse_url(url)
    if (rm_query) {
        parsed_url$query <- NULL
    }
    if (!is.null(replace_domains)) {
        parsed_url$hostname <- sample(replace_domains, 1)
    }
    return(httr::build_url(parsed_url))
}
