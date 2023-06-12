
require(webdriver)
require(magrittr)
pjs_instance <- run_phantomjs()
pjs_session <- Session$new(port = pjs_instance$port)


#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

#function for geting links from page
neop_getlink <- function(html){

  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//header//h3//a") %>% 
    rvest::html_text(., trim = TRUE) -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//header//h3//a") %>% 
    rvest::html_attr("href")  -> item_link
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//header//span[contains(@class, 'entry-meta-date updated')]") %>% 
    rvest::html_text(., trim = TRUE) %>% 
    stringr::str_replace(., "März", "March") %>%
    lubridate::dmy() -> item_pubdate
    
    df <- data.frame(item_title, item_link, item_pubdate)
    return(df)
}

neop_getlink_url <- function(url){
  pjs_session$go(url)
  print(url)
  return(neop_getlink(pjs_session$getSource()))
}

neop_go_thr_archive <- function(startdate){
  i <- 1
  j <- 1
  k <- 0
  valid_links <- data.frame()
  
  while (i > 0) {
    neop_getlink_url(paste0("https://www.neopresse.com/2023/page/", j, "/")) %>% 
      subset(item_pubdate>=as.Date(startdate)) -> subset_links
    i <- nrow(subset_links)
    j <- j + 1
    k <- k + 1
    valid_links <- rbind(valid_links, subset_links)
    if(k > 50){
      Sys.sleep(60)
      k <- 0
    }
  }
  return(valid_links)
}

valid_links <- neop_go_thr_archive("2021-12-01")


valid_links %>% dplyr::rename(title = item_title, link = item_link) %>% 
  dplyr::mutate(pub = "NeoPresse", description = NA, pubdate = NA) %>%
  dplyr::select(pub, link, pubdate, title, description) -> valid_links

saveRDS(valid_links, "Neopresse_2.RDS")



