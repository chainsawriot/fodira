
require(webdriver)
require(magrittr)
pjs_instance <- run_phantomjs()
pjs_session <- Session$new(port = pjs_instance$port)


#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

#function for geting links from page
tagesschau_getlink <- function(html){

  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//span[contains(@class, 'teaser-xs__headline')]") %>% 
    rvest::html_text(., trim = TRUE) -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//a[contains(@class, 'teaser-xs__link')]") %>% 
    rvest::html_attr("href")  -> item_link
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//span[contains(@class, 'teaser-xs__date')]") %>% 
    rvest::html_text(., trim = TRUE) %>% 
    as.Date(., tryFormat = c("%d.%m.%Y - %H:%M Uhr")) -> item_pubdate
  
    df <- data.frame(item_title, item_link, item_pubdate)
    return(df)
}

tagesschau_getlink_url <- function(url){
  pjs_session$go(url)
  print(url)
  return(tagesschau_getlink(pjs_session$getSource()))
}


tagesschau_go_thr_archive <- function(startdate){
  V1<-seq(as.Date(startdate), Sys.Date(), by="days")
  
  V1 %>% as.character() %>%
    paste0("https://www.tagesschau.de/archiv/?datum=", .) %>%
    purrr::map_df(~tagesschau_getlink_url(.)) -> valid_links
  
  return(valid_links)
}


valid_links <- tagesschau_go_thr_archive("2022-01-01")


