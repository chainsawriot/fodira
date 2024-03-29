
require(webdriver)
require(magrittr)
pjs_instance <- run_phantomjs()
pjs_session <- Session$new(port = pjs_instance$port)

#function for geting links from page
#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")


nachdenkseiten_getlink <- function(html){

  html <- pjs_session$getSource()
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//article//h2/a") %>% 
    rvest::html_text(., trim = TRUE) -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//article//h2/a") %>% 
    rvest::html_attr("href") -> item_link
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//article//span[contains(@class, 'postMeta')]") %>% 
    rvest::html_text(., trim = TRUE) %>% 
    stringr::str_replace(., "März", "March") %>%
    lubridate::dmy_hm() -> item_pubdate
    
    df <- data.frame(item_title, item_link, item_pubdate)
    return(df)
}

nachdenkseiten_getlink_url <- function(url){
  pjs_session$go(url)
  print(url)
  return(nachdenkseiten_getlink(pjs_session$getSource()))
}

nachdenkseiten_go_thr_archive <-  function(startdate){
  i <- 1
  j <- 1
  valid_links <- data.frame()
  while (i > 0) {
    nachdenkseiten_getlink_url(paste0("https://www.nachdenkseiten.de/?paged=", j)) %>% 
      subset(item_pubdate>=as.Date(startdate)) -> subset_links
    i <- nrow(subset_links)
    j <- j + 1
    valid_links <- rbind(valid_links, subset_links)
  }
  return(valid_links)
}

valid_links <- nachdenkseiten_go_thr_archive("2021-12-01")


valid_links %>% dplyr::rename(title = item_title, link = item_link, pubdate = item_pubdate) %>% 
  dplyr::mutate(pub = "Nachdenkseiten", description = NA) %>%
  dplyr::select(pub, link, pubdate, title, description) -> valid_links


saveRDS(valid_links, "Nachdenkseiten.RDS")





