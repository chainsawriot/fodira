
require(webdriver)
require(magrittr)
pjs_instance <- run_phantomjs()
pjs_session <- Session$new(port = pjs_instance$port)

#function for geting links from page
#Sys.setlocale("LC_TIME", "C")
#Sys.setlocale("LC_TIME", "de_DE")


klggkl_getlink <- function(html){

  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//article[contains(@class, 'article  width-33 ')]//h3/a") %>% 
    rvest::html_text(., trim = TRUE) -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//article[contains(@class, 'article  width-33 ')]//h3/a") %>% 
    rvest::html_attr("href") -> item_link
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//article[contains(@class, 'article  width-33 ')]//time") %>% 
    rvest::html_attr("datetime") %>% 
    as.Date() -> item_pubdate
    
    df <- data.frame(item_title, item_link, item_pubdate)
    return(df)
}

klggkl_getlink_url <- function(url){
  pjs_session$go(url)
  print(url)
  return(klggkl_getlink(pjs_session$getSource()))
}

klggkl_go_thr_columns <- function(rubrik, startdate){
  i <- 1
  j <- 1
  valid_links <- data.frame()
  while (i > 0) {
    klggkl_getlink_url(paste0("https://www.klassegegenklasse.org/kategorie/", rubrik, "/page/", j, "/")) %>% 
    subset(item_pubdate>=as.Date(startdate)) -> subset_links
    i <- nrow(subset_links)
    j <- j + 1
    valid_links <- rbind(valid_links, subset_links)
  }
  return(valid_links)
}

c("world", "deutschland", "our_class", "antirassismus", "jugend", "frauen-und-lgbti",
  "geschichte-und-kultur", "hintergruende") %>% 
  purrr::map_dfr(~klggkl_go_thr_columns(., startdate = "2022-01-01")) -> valid_links

valid_links <- dplyr::distinct(valid_links)


valid_links %>% dplyr::rename(title = item_title, link = item_link, pubdate = item_pubdate) %>% 
  dplyr::mutate(pub = "Klasse gegen Klasse", description = NA) %>%
  dplyr::select(pub, link, pubdate, title, description) -> valid_links


saveRDS(valid_links, "Klasse gegen Klasse.RDS")



