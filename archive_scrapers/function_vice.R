
require(webdriver)
require(magrittr)
pjs_instance <- run_phantomjs()
pjs_session <- Session$new(port = pjs_instance$port)

#function for geting links from page
#Sys.setlocale("LC_TIME", "C")
#Sys.setlocale("LC_TIME", "de_DE")


vice_getlink <- function(html){
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'grid__wrapper grd-row')]//a//h2") %>% 
    rvest::html_text(., trim = TRUE) -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'grid__wrapper grd-row')]//a") %>% 
    rvest::html_attr("href") -> item_link
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'grid__wrapper grd-row')]//div[contains(@class, 'dsp-inline-xs hed-xxs canonical__date hed-xxs')]") %>% 
    rvest::html_text(., trim = TRUE) -> item_pubdate1
  
  item_pubdate1 %>% 
    stringr::str_replace(., "heute", format(Sys.Date(), "%m.%d.%y")) %>% 
    stringr::str_replace(., "vor einem Tag", format(Sys.Date()-1, "%m.%d.%y")) %>% 
    stringr::str_replace(., "vor 2 Tagen", format(Sys.Date()-2, "%m.%d.%y")) %>% 
    stringr::str_replace(., "vor 3 Tagen", format(Sys.Date()-3, "%m.%d.%y")) %>%
    as.Date(., format = "%m.%d.%y") -> item_pubdate
    
    df <- data.frame(item_title, item_link, item_pubdate)
    return(df)
}

vice_getlink_url <- function(url){
  pjs_session$go(url)
  print(url)
  return(vice_getlink(pjs_session$getSource()))
}

vice_go_thr_archive <- function(startdate){
  i <- 1
  j <- 1
  valid_links <- data.frame()
  while (i > 0) {
    vice_getlink_url(paste0("https://www.vice.com/de/read?page=", j)) %>% 
    subset(item_pubdate>=as.Date(startdate)) -> subset_links
    i <- nrow(subset_links)
    j <- j + 1
    valid_links <- rbind(valid_links, subset_links)
  }
  return(valid_links)
}

vice_go_thr_archive(startdate = "2022-01-01") -> valid_links

valid_links <- dplyr::distinct(valid_links)



