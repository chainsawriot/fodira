
require(webdriver)
require(magrittr)
pjs_instance <- run_phantomjs()
pjs_session <- Session$new(port = pjs_instance$port)


#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

#function for geting links from page
tichy_getlink <- function(html){

  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'entry-content')]//div[contains(@class, 'category-content-title')]/a") %>% 
    rvest::html_text(., trim = TRUE) -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'entry-content')]//div[contains(@class, 'category-content-title')]/a") %>% 
    rvest::html_attr("href")  -> item_link
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'entry-content')]//span[contains(@class, 'date')]") %>% 
    rvest::html_text(., trim = TRUE) %>% 
    as.Date(., tryFormat = c("%d. %B %Y")) -> item_pubdate
    
    df <- data.frame(item_title, item_link, item_pubdate)
    return(df)
}

tichy_getlink_url <- function(url){
  pjs_session$go(url)
  print(url)
  return(tichy_getlink(pjs_session$getSource()))
}

tichy_getlink_url("https://www.tichyseinblick.de/tichys-einblick/")

tichy_go_thr_columns <- function(rubrik, startdate){
  i <- 1
  j <- 1
  valid_links <- data.frame()
  while (i > 0) {
    tichy_getlink_url(paste0("https://www.tichyseinblick.de/", rubrik, "/page/", j, "/")) %>% 
      subset(item_pubdate>=as.Date(startdate)) -> subset_links
    i <- nrow(subset_links)
    j <- j + 1
    valid_links <- rbind(valid_links, subset_links)
  }
  return(valid_links)
}


c("tichys-einblick", "kolumnen", "gastbeitrag", "daili-es-sentials", 
  "meinungen", "feuilleton", "wirtschaft") %>% 
  purrr::map_dfr(~tichy_go_thr_columns(., startdate = "2022-01-01")) -> valid_links




