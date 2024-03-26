require(RSelenium)
require(magrittr)
rD <- RSelenium::rsDriver(browser = "firefox", port = sample(c(5678L, 5679L, 5680L, 5681L, 5682L), size = 1), check = FALSE, verbose = FALSE)
remDr <- rD[["client"]]

#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

#function for geting links from page
ppq_getlink <- function(html){
  #html <- remDr$getPageSource()[[1]]
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'blog-posts hfeed')]//h2[contains(@class, 'date-header')]") %>% length() -> j
  
  item_title <- c()
  item_link <- c()
  item_pubdate <- c(as.Date("2022-01-01"))
  
  for (i in 1:j) {
    rvest::read_html(html) %>% 
      rvest::html_elements(xpath = paste0("//div[contains(@class, 'date-outer')][", 
                                          i, "]//h3[contains(@class, 'post-title entry-title')][1]/a[1]")) %>% 
      rvest::html_text(., trim = TRUE) %>% c(item_title, .)-> item_title
    
    #//div[contains(@class, 'blog-posts hfeed')]
    
    rvest::read_html(html) %>% 
      rvest::html_elements(xpath = paste0("//div[contains(@class, 'date-outer')][", 
                                          i, "]//h3[contains(@class, 'post-title entry-title')]/a")) %>% 
      rvest::html_attr("href") -> item_link_ 
    item_link <- c(item_link, item_link_)
    rvest::read_html(html) %>% 
      rvest::html_elements(xpath = paste0("//div[contains(@class, 'date-outer')][", 
                                          i, "]/h2[contains(@class, 'date-header')][1]")) %>% 
      rvest::html_text(., trim = TRUE) %>% stringr::str_match(., "[0-9]+[.] [A-Za-zä]+ [0-9]+") %>%
      stringr::str_replace(., "März", "March") %>% lubridate::dmy() %>% 
      rep(. , length(item_link_)) -> item_pubdate_
    item_pubdate <- c(item_pubdate, item_pubdate_)
  }
    df <- data.frame(item_title, item_link, item_pubdate = item_pubdate[2:(length(item_link)+1)])
    return(df)
}
# 
# ppq_getlink_url <- function(url){
#   remDr$navigate(url)
#   print(url)
#   return(ppq_getlink(remDr$getPageSource()[[1]]))
# }

ppq_go_thr_archive <- function(startdate){
  valid_links <- data.frame()
  remDr$navigate("https://www.politplatschquatsch.com/2024")
  ppq_getlink(remDr$getPageSource()[[1]]) %>% subset(., item_pubdate >= as.Date(startdate)) -> subset_links
  i <- nrow(subset_links)
  valid_links <- rbind(valid_links, subset_links)
  while (i > 0) {
    webElem <- remDr$findElement(using = "css", "span[id='blog-pager-older-link']")
    webElem$clickElement()
    ppq_getlink(remDr$getPageSource()[[1]]) %>% subset(., item_pubdate >= as.Date(startdate)) -> subset_links
    i <- nrow(subset_links)
    valid_links <- rbind(valid_links, subset_links)
  } 
  return(valid_links)
}

ppq_go_thr_archive("2022-01-01") -> valid_links

valid_links %>% dplyr::rename(title = item_title, link = item_link, pubdate = item_pubdate) %>% 
  dplyr::mutate(pub = "Politplatschquatsch", description = NA) %>%
  dplyr::select(pub, link, pubdate, title, description) -> valid_links

saveRDS(valid_links, "Politplatschquatsch2.RDS")

remDr$close()
z <- rD$server$stop()
