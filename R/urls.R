#' Modify urls
#'
#' Modify urls, either by removing the query part, or the domain names.
#' @param url character, input url
#' @param rm_query logical, whether to remove the queries (?) and fragments (#)
#' @param replace_domains character vector, when is not NULL, one domain from this vector is chosen to replace the hostname of the input url
#' @return a modified url
#' @author Chung-hong Chan
#' @importFrom rlang .data
#' @export
modify_url <- function(url, rm_query = TRUE, replace_domains = NULL) {
    purrr::map_chr(url, .modify_url, rm_query = rm_query, replace_domains = replace_domains)
}

## Being vectorized above
.modify_url <- function(url, rm_query = TRUE, replace_domains = NULL) {
    parsed_url <- httr::parse_url(url)
    if (rm_query) {
        parsed_url$query <- NULL
        parsed_url$fragment <- NULL
    }
    if (!is.null(replace_domains)) {
        parsed_url$hostname <- sample(replace_domains, 1)
    }
    return(httr::build_url(parsed_url))
}

#' Harmonize output from either rss aggregator or archive scrapers
#'
#' This harmonizes the output by cleaning the links of some selected outlets and remove entries with duplicated links. The cleaning involves the removal of query strings in links using [modify_url()].
#' 
#' @param output data.frame in the standardized docdb format
#' @param pubs_require_cleaning a vector of outlets requiring cleaning
#' @param remove_duplicates whether to remove deplicated entries with duplicated links
#' @return a cleaned data.frame in the docdb format
#' @export
harmonize_output <- function(output, pubs_require_cleaning  = c("nordbayern.de", "Spiegel", "Bild", "Berliner Zeitung", "Stuttgarter Zeitung", "HNA", "Merkur", "Stern", "Klasse gegen Klasse", "Tagesspiegel", "DieUnbestechlichen", "Heute"), remove_duplicates = TRUE) {
    output$link <- dplyr::case_when(output$pub %in% pubs_require_cleaning ~ modify_url(output$link, rm_query = TRUE),
                                    TRUE ~ output$link)
    if (remove_duplicates) {
        return(dplyr::distinct(output, .data$link, .keep_all = TRUE))
    } else {
        return(output)
    }
}
