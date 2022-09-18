require(academictwitteR)
require(dplyr)

dir <- academictwitteR:::.gen_random_dir()
x <- academictwitteR:::get_all_tweets("from:jacobinmag_de", start_tweets = "2021-12-31T00:00:00Z", end_tweets = "2022-09-02T00:00:00Z", n = Inf, bind_tweets = FALSE, data_path = dir, is_retweet = FALSE)

res <- academictwitteR:::bind_tweets(dir, output_format = "tidy")
all_urls <- stringr::str_extract_all(res$text, "https://t.co/[a-zA-Z0-9]+")


.resolve <- function(tcourl){
  cat(tcourl)
  httr::HEAD(tcourl)$url
}

res$resolved_urls <- purrr::map(all_urls, ~purrr::map_chr(., .resolve))

.sub <- function(x) {
    z <- stringr::str_subset(x, "^https://jacobin\\.de/artikel")
    res <- z[1]
    if (is.null(res)) {
        return(NA)
    }
    res
}

res$url <- res$resolved_urls %>% purrr::map_chr(.sub)

res %>% select(created_at, text, url) %>% rename(title = `text`, link = url, pubdate = created_at) %>% 
  mutate(pubdate = lubridate::ymd_hms(pubdate)) %>% mutate(pub = "Jacobin", description = NA) %>% 
  arrange(pubdate) %>% select(pub, link, pubdate, title, description) -> valid_links
  
saveRDS(valid_links, "jacobin_archive.RDS")
