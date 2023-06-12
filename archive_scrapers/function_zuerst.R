
require(webdriver)
require(magrittr)
pjs_instance <- run_phantomjs()
pjs_session <- Session$new(port = pjs_instance$port)


#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

#function for geting links from page
zuerst_getlink <- function(html){

  html <- pjs_session$getSource()
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'bl2page-archive')]//h2//a") %>% 
    rvest::html_text(., trim = TRUE) -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'bl2page-archive')]//h2//a") %>% 
    rvest::html_attr("href")  -> item_link
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'bl2page-archive')]//div[contains(@class, 'bl2page-info')]") %>% 
    rvest::html_text(., trim = TRUE) %>% 
    stringr::str_replace(., "März", "March") %>%
    lubridate::dmy() -> item_pubdate
    
    df <- data.frame(item_title, item_link, item_pubdate)
    return(df)
}

zuerst_getlink_url <- function(url){
  pjs_session$go(url)
  print(url)
  return(zuerst_getlink(pjs_session$getSource()))
}

zuerst_go_thr_archive <- function(startdate){
  i <- 1
  j <- 1
  valid_links <- data.frame()
  while (i > 0) {
    zuerst_getlink_url(paste0("https://zuerst.de/page/", j, "/")) %>% 
      subset(item_pubdate>=as.Date(startdate)) -> subset_links
    i <- nrow(subset_links)
    j <- j + 1
    valid_links <- rbind(valid_links, subset_links)
  }
  return(valid_links)
}

valid_links <- zuerst_go_thr_archive("2022-08-01")


valid_links %>% dplyr::rename(title = item_title, link = item_link, pubdate = item_pubdate) %>% 
  dplyr::mutate(pub = "Zuerst", description = NA) %>%
  dplyr::select(pub, link, pubdate, title, description) -> valid_links


saveRDS(valid_links, "Zuerst.RDS")


