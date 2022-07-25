
require(webdriver)
require(magrittr)
pjs_instance <- run_phantomjs()
pjs_session <- Session$new(port = pjs_instance$port)


#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

#function for geting links from page
focus_getlink <- function(html){

  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'rightCol')]//a") %>% 
    rvest::html_text(., trim = TRUE) -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'rightCol')]//a") %>% 
    rvest::html_attr("href") -> item_link
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//span[contains(@class, 'currentDate')]") %>%
    rvest::html_text(., trim = TRUE) -> date0
  
  as.Date(paste(date0[3], date0[2], date0[1]), tryFormat = c("%d %B %Y")) -> item_pubdate
  
  if(length(item_link)>0){
    df <- data.frame(item_title, item_link, item_pubdate)
  } else {
    df <- data.frame()
  }
    
    return(df)
}

focus_getlink_url <- function(url){
  pjs_session$go(url)
  print(url)
  return(focus_getlink(pjs_session$getSource()))
}

focus_go_thr_archive <- function(rubrik, startdate){
  seq(as.Date(startdate), Sys.Date(), by="days") %>% 
    format.Date(format="%d-%m-%Y") -> V1
  
  V1 %>%
    paste0("https://www.focus.de/archiv/", rubrik, "/", ., "/") %>%
    purrr::map_df(~focus_getlink_url(.)) -> valid_links
  
  return(valid_links)
}


c("familie", "immobilien", "reisen", "digital", "gesundheit", 
  "wissen", "kultur", "finanzen", "politik", "panorama", "sport", 
  "auto") %>% purrr::map_df(~focus_go_thr_archive(., startdate = "2022-01-01")) -> valid_links



