#' Modify urls
#'
#' Modify urls, either by removing the query part, or the domain names.
#' @param url character, input url
#' @param rm_query logical, whether to remove the query part
#' @param replace_domains character vector, when is not NULL, one domain from this vector is chosen to replace the hostname of the input url
#' @return a modified url
#' @author Chung-hong Chan
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
