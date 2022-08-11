require(academictwitteR)
require(dplyr)
require(magrittr)

dir <- academictwitteR:::.gen_random_dir()
x <- academictwitteR:::get_all_tweets("from:StZ_NEWS", start_tweets = "2021-12-31T00:00:00Z", end_tweets = Sys.time(), n = Inf, bind_tweets = FALSE, data_path = dir, is_retweet = FALSE)

res <- academictwitteR:::bind_tweets(dir, output_format = "tidy")
all_urls <- stringr::str_extract_all(res$text, "https://t.co/[a-zA-Z0-9]+")


.resolve <- function(tcourl){
  cat(tcourl)
    httr::HEAD(tcourl)$url
}



#resolved_urls_01 <- purrr::map(all_urls[1:1000], ~purrr::map_chr(., .resolve))
res$resolved_urls <- purrr::map(all_urls, ~purrr::map_chr(., .resolve))

.sub <- function(x) {
    z <- stringr::str_subset(x, "^https://www.stuttgarter-zeitung\\.de/")
    res <- z[1]
    if (is.null(res)) {
        return(NA)
    }
    res
}

#url_1 <- resolved_urls_01 %>% purrr::map_chr(.sub)

res$url <- res$resolved_urls %>% purrr::map_chr(.sub)

res %>% select(created_at, text, url) %>% rename(title = `text`, link = url, pubdate = created_at) %>% mutate(pubdate = lubridate::ymd_hms(pubdate)) %>% mutate(pub = "Stuttgarter Zeitung", description = NA) %>% arrange(pubdate) %>% select(pub, link, pubdate, title, description) %>% saveRDS("stuttgarter_Z_archive.RDS")
