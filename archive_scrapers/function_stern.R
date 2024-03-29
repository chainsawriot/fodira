
require(webdriver)
require(magrittr)
pjs_instance <- run_phantomjs()
pjs_session <- Session$new(port = pjs_instance$port)


#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

#function for geting links from page
stern_getlink <- function(html){

  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'group__items')]//article//a") %>% 
    rvest::html_text(., trim = TRUE) -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'group__items')]//article//a") %>% 
    rvest::html_attr("href")  -> item_link
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'group__items')]//time") %>% 
    rvest::html_text(., trim = TRUE) %>% stringr::str_extract("[0-9]+[.][0-9]+[.][0-9]+") %>%
    as.Date(., tryFormat = c("%d.%m.%Y")) -> item_pubdate
  
  if(length(item_pubdate)< length(item_link)){
    item_pubdate <- c(item_pubdate, item_pubdate[1:(length(item_link)-length(item_pubdate))])
  }
  
    df <- data.frame(item_title, item_link, item_pubdate)
    return(df)
}

stern_getlink_url <- function(url){
  pjs_session$go(url)
  print(url)
  return(stern_getlink(pjs_session$getSource()))
}

stern_go_thr_archive <- function(rubrik, startdate){
  j <- 0
  i <- 1

  seq(as.Date(startdate), Sys.Date(), by="month") %>% format.Date(., format = "?month=%m&year=%Y") %>%
    as.character() %>% 
    stringr::str_replace_all("=0", "=") -> V1
  print(V1)
  valid_links <- data.frame()
  
  for (k in (1:length(V1))) {
    j <- 0
    i <- 1
    while (i > 0) {
      
      paste0("https://www.stern.de/", rubrik, "/archiv/", V1[k], "&pageNum=", j) %>%
        purrr::map_df(~stern_getlink_url(.)) -> subset_links
      i <- nrow(subset_links)
      j <- j + 1
      valid_links <- rbind(valid_links, subset_links)
    }
  }
    

  return(valid_links)
}


c("politik", "gesellschaft", "panorama", "kultur", "lifestyle", "digital",
  "wirtschaft", "sport", "gesundheit", "genuss", "reise", "familie", 
  "auto") %>% purrr::map_df(~stern_go_thr_archive(., startdate = "2021-12-01")) -> valid_links

valid_links %>% dplyr::rename(title = item_title, link = item_link, pubdate = item_pubdate) %>% 
  dplyr::mutate(pub = "Stern", description = NA) %>%
  dplyr::select(pub, link, pubdate, title, description) -> valid_links

saveRDS(valid_links, "Stern.RDS")

