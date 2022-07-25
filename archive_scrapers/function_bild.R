
require(webdriver)
require(magrittr)
pjs_instance <- run_phantomjs()
pjs_session <- Session$new(port = pjs_instance$port)


#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

#function for geting links from page
bild_getlink <- function(html){

  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'txt')]//p/a[1]") %>% 
    rvest::html_text(., trim = TRUE) -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'txt')]//p/a[1]") %>% 
    rvest::html_attr("href")  %>% paste0("https://www.bild.de", .)-> item_link
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//span[contains(@class, 'headline')]") %>% 
    rvest::html_text(., trim = TRUE) %>% stringr::str_extract("[0-9]+[.][0-9]+[.][0-9]+") %>%
    as.Date(., tryFormat = c("%d.%m.%Y")) -> item_pubdate
  
    df <- data.frame(item_title, item_link, item_pubdate)
    return(df[4:nrow(df),])
}

bild_getlink_url <- function(url){
  pjs_session$go(url)
  print(url)
  return(bild_getlink(pjs_session$getSource()))
}

bild_go_thr_archive <- function(startdate){
  seq(as.Date(startdate), Sys.Date(), by="days") %>% 
    as.character() %>% 
    stringr::str_replace_all("-0", "-") %>%
    stringr::str_replace_all("-", "/") -> V1
  
  V1 %>%
    paste0("https://www.bild.de/archive/", ., "/") %>%
    purrr::map_df(~bild_getlink_url(.)) -> valid_links
  
  return(valid_links)
}


valid_links <- bild_go_thr_archive("2022-01-01")


