require(RSelenium)
require(magrittr)

#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

#function for geting links from page
f21_getlink <- function(html){
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'fusion-post-content-wrapper')]//h2/a") %>% 
    rvest::html_text(trim = TRUE) -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'fusion-post-content-wrapper')]//h2/a") %>% 
    rvest::html_attr("href") -> item_link
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'fusion-post-content-wrapper')]//p[contains(@class, 'fusion-single-line-meta')]") %>% 
    rvest::html_text(., trim = TRUE) %>%
  stringr::str_extract("[0-9]+[-][0-9]+[-][0-9]+") %>%
    as.Date(.) -> item_pubdate
  
  df <- data.frame(item_title, item_link, item_pubdate)
  return(df)
}


f21_go_thr_archive <- function(startdate){
  i <- 1
  j <- 1
  rD <- RSelenium::rsDriver(browser = "firefox", port = sample(c(5678L, 5679L, 5680L, 5681L, 5682L), size = 1), check = FALSE, verbose = FALSE)
  remDr <- rD[["client"]]
  Sys.sleep(5)
  valid_links <- data.frame()
  
  while (i >0) {
    remDr$navigate(paste0("https://free21.org/page/", j, "/"))
    print(paste0("https://free21.org/page/", j, "/"))
    f21_getlink(remDr$getPageSource()[[1]]) %>% subset(., item_pubdate >= as.Date(startdate))-> subset_links
    valid_links <- rbind(valid_links, subset_links)
    i <- nrow(subset_links)
    j <- j+1
  }
  remDr$close()
  z <- rD$server$stop()
  return(valid_links)
}


f21_go_thr_archive("2021-12-01") -> valid_links

valid_links %>% dplyr::rename(title = item_title, link = item_link, pubdate = item_pubdate) %>% 
  dplyr::mutate(pub = "Free21", description = NA) %>%
  dplyr::select(pub, link, pubdate, title, description) -> valid_links


saveRDS(valid_links, "Free21.RDS")
