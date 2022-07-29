
require(webdriver)
require(magrittr)
pjs_instance <- run_phantomjs()
pjs_session <- Session$new(port = pjs_instance$port)


#Sys.setlocale("LC_TIME", "C")
#Sys.setlocale("LC_TIME", "de_DE")

#function for geting links from page
ossietzky_getlink <- function(html){
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//h2/a") %>% 
    rvest::html_text(., trim = TRUE) -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//h2/a") %>% 
    rvest::html_attr("href")  -> item_link
    
    df <- data.frame(item_title, item_link)
    return(df)
}

ossietzky_getlink_url <- function(url){
  pjs_session$go(url)
  print(url)
  return(ossietzky_getlink(pjs_session$getSource()))
}

ossietzky_go_thr_archive <- function(){
  pjs_session$go("https://www.ossietzky.net/zeitschrift/")
  
  rvest::read_html(pjs_session$getSource()) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'LDA_ausgabe')]/a") %>% 
    rvest::html_attr("href") -> ausgaben
  ausgaben <- ausgaben[stringr::str_detect(ausgaben,"202[234]")]
  
  ausgaben %>% as.character() %>%
    paste0("https://www.ossietzky.net", ., "/") %>%
    purrr::map_df(~ossietzky_getlink_url(.)) -> valid_links
  
  return(valid_links)
}

valid_links <- ossietzky_go_thr_archive()


