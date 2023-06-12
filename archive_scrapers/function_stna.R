require(academictwitteR)
require(dplyr)
require(magrittr)

dir <- academictwitteR:::.gen_random_dir()
x <- academictwitteR:::get_all_tweets("from:StN_NEWS", start_tweets = "2021-12-31T00:00:00Z", end_tweets = "2023-04-24T00:00:00Z", n = Inf, bind_tweets = FALSE, data_path = dir, is_retweet = FALSE)

res <- academictwitteR:::bind_tweets(dir, output_format = "tidy")
res$text <- stringr::str_replace_all(res$text, "https://t.co/HRXsfq4R0Q", "")
all_urls <- stringr::str_extract_all(res$text, "https://t.co/[a-zA-Z0-9]+")


.resolve <- function(tcourl){
  cat(tcourl)
    httr::HEAD(tcourl)$url
}



#resolved_urls_01 <- purrr::map(all_urls[1:1000], ~purrr::map_chr(., .resolve))
res$resolved_urls <- purrr::map(all_urls, ~purrr::map_chr(., .resolve))

all_urls[7829] <- "https://t.co/yK9jk9jJCW"

stringr::str_which(all_urls, "https://t.co/XCVD9EJRaw")

.sub <- function(x) {
    z <- stringr::str_subset(x, "^https://www.stuttgarter-nachrichten\\.de/")
    res <- z[1]
    if (is.null(res)) {
        return(NA)
    }
    res
}

#url_1 <- resolved_urls_01 %>% purrr::map_chr(.sub)



res$url <- res$resolved_urls %>% purrr::map_chr(.sub)

res %>% select(created_at, text, url) %>% rename(title = `text`, link = url, pubdate = created_at) %>% 
  mutate(pubdate = lubridate::ymd_hms(pubdate)) %>% mutate(pub = "Stuttgarter Zeitung", description = NA) %>% 
  arrange(pubdate) %>% select(pub, link, pubdate, title, description) -> valid_links

#### split by date!

valid_links <- valid_links[!is.na(valid_links$link),]

valid_links_1 <- subset(valid_links, pubdate > as.Date("2022-09-08"))

valid_links_2 <- subset(valid_links, pubdate <= as.Date("2022-09-08"))

saveRDS(valid_links_1, "stuttgarter_Z_archive_new.RDS")

saveRDS(valid_links_2, "stuttgarter_Z_archive_old.RDS")

valid_links_1$false_link <- valid_links_1$link

valid_links_1$link <- stringr::str_remove(valid_links_1$false_link, "[?].*")


valid_links_2$false_link <- valid_links_2$link

valid_links_2$link <- stringr::str_remove(valid_links_2$false_link, "[?].*")

saveRDS(valid_links_1, "stuttgarter_Z_archive_new.RDS")

saveRDS(valid_links_2, "stuttgarter_Z_archive_old.RDS")
