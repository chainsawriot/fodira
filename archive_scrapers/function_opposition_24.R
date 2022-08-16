
require(webdriver)
require(magrittr)

pjs_instance <- run_phantomjs()
pjs_session <- Session$new(port = pjs_instance$port)

#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

#function for geting links from page
opp_getlink <- function(html){

  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//h2//a") %>% 
    rvest::html_text(., trim = TRUE) -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//h2//a") %>% 
    rvest::html_attr("href")  -> item_link
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//time") %>% 
    rvest::html_text(., trim = TRUE) %>%
    stringr::str_replace(., "März", "March") %>%
    lubridate::dmy() -> item_pubdate
    
    df <- data.frame(item_title, item_link, item_pubdate)
    return(df)
}

opp_getlink_url <- function(url){
  pjs_session$go(url)
  print(url)
  return(opp_getlink(pjs_session$getSource()))
}

opp_go_thr_archive <- function(year, lastpage){
  i <- 1
  j <- 1
  k <- 0
  valid_links <- data.frame()
  while (j <= lastpage) {
    opp_getlink_url(paste0("https://opposition24.com/", year, "/page/", j, "/")) -> subset_links
    j <- j + 1

    valid_links <- rbind(valid_links, subset_links)

  }
  return(valid_links)
}

valid_links <- opp_go_thr_archive("2022", 97)


valid_links %>% dplyr::rename(title = item_title, link = item_link, pubdate = item_pubdate) %>% 
  dplyr::mutate(pub = "Opposition24", description = NA) %>%
  dplyr::select(pub, link, pubdate, title, description) -> valid_links


saveRDS(valid_links, "Opposition24.RDS")



