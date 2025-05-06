
require(RSelenium)
require(magrittr)
rD <- RSelenium::rsDriver(browser = "firefox", port = sample(c(5678L, 5679L, 5680L, 5681L#, 5682L
                                                               ), size = 1), check = FALSE, verbose = FALSE)
remDr <- rD[["client"]]

#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

#function for geting links from page
taz_getlink <- function(html){
  
  html <- remDr$getPageSource()[[1]]
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//main//section//p[contains(@class, 'headline')]") %>% 
    rvest::html_text(., trim = TRUE) -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//main//section//div[contains(@class, 'article-teaser')]/div[contains(@class, 'column')]/div[contains(@class, 'mobile-order-1')]/a[contains(@class, 'teaser-link')]") %>% 
    rvest::html_attr("href") -> item_link
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//span[contains(@class, 'typo-link-grey-onpage')]|//p[contains(@class, 'typo-link-grey-onpage')]") %>%
    rvest::html_text(., trim = TRUE) %>% lubridate::dmy() -> pubdate1
  
  pubdate1[!is.na(pubdate1)]  -> item_pubdate

  while(length(item_pubdate) < length(item_title)){
    item_pubdate <- c(item_pubdate, item_pubdate[1])
  }

    df <- data.frame(item_title, item_link, item_pubdate)
    
    return(df)
}

taz_getlink_url <- function(url){
  remDr$navigate(url)
  print(url)
  df <- data.frame()
  
  remDr$getPageSource()[[1]] %>% rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//nav[contains(@class, 'pagination ')]//li[last()]") %>%
    rvest::html_text(., trim = TRUE) -> n

  for (i in 1:as.numeric(n)) {
    remDr$navigate(paste0(url, "/?search_page=", (i-1)))
    df <- taz_getlink(remDr$getPageSource()[[1]])
  }
  
  return(df)
}

# taz_getlink_url("https://taz.de/!s=&eTagAb=2022-04-23&eTagBis=2022-04-25/")

taz_go_thr_archive <- function(startdate, enddate){
  seq(as.Date(startdate)-1, as.Date(enddate)-2, by="days") %>% 
    format.Date(format="%Y-%m-%d") -> V1
  seq(as.Date(startdate)+1, as.Date(enddate), by="days") %>% 
    format.Date(format="%Y-%m-%d") -> V2
  
  paste0("!s=&eTagAb=", V1, "&eTagBis=", V2, "/") -> V3
  
  V3 %>%
    paste0("https://www.taz.de/", .) %>%
    purrr::map_df(~taz_getlink_url(.)) -> valid_links
  
  return(valid_links)
}


taz_go_thr_archive(startdate = "2023-01-01", enddate = "2023-06-01") -> valid_links1

taz_go_thr_archive(startdate = "2023-06-01", enddate = "2023-12-01") -> valid_links2

taz_go_thr_archive(startdate = "2023-12-01", enddate = "2024-06-01") -> valid_links3

taz_go_thr_archive(startdate = "2024-06-01", enddate = "2024-12-01") -> valid_links4

taz_go_thr_archive(startdate = "2024-12-01", enddate = Sys.Date()) -> valid_links5



valid_links <- dplyr::distinct(rbind(valid_links1, valid_links2, valid_links3,
                               valid_links4, valid_links5))

remDr$close()
z <- rD$server$stop()


valid_links %>% dplyr::rename(title = item_title, link = item_link, pubdate = item_pubdate) %>% 
  dplyr::mutate(pub = "TAZ", description = NA) %>%
  dplyr::select(pub, link, pubdate, title, description) -> valid_links

saveRDS(valid_links, "TAZ_auflösen.RDS")
