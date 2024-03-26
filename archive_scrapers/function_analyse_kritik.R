
require(webdriver)
require(magrittr)
pjs_instance <- run_phantomjs()
pjs_session <- Session$new(port = pjs_instance$port)


#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

#function for geting links from page
ak_getlink <- function(html){

  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//li[contains(@class, 'article-item article-item--type-issue')]//h3//a") %>% 
    rvest::html_text(., trim = TRUE) -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//li[contains(@class, 'article-item article-item--type-issue')]//h3//a") %>% 
    rvest::html_attr("href")  -> item_link
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//small[contains(@class, 'issue-header__publication-date t-small-sl s-m-b-8')]") %>% 
    rvest::html_text(., trim = TRUE) %>% 
    stringr::str_replace(., "März", "March") %>%
    lubridate::dmy() -> item_pubdate
    
    df <- data.frame(item_title, item_link, item_pubdate)
    return(df)
}

ak_getlink_url <- function(url){
  pjs_session$go(url)
  print(url)
  return(ak_getlink(pjs_session$getSource()))
}


ak_go_thr_archive <- function(start_issue, end_issue){
  V1<- end_issue:start_issue
  
  V1 %>% as.character() %>%
    paste0("https://www.akweb.de/ausgaben/", .) %>%
    purrr::map_df(~ak_getlink_url(.)) -> valid_links
  
  return(valid_links)
}

valid_links <- ak_go_thr_archive("677", "700")


valid_links %>% dplyr::rename(title = item_title, link = item_link, pubdate = item_pubdate) %>% 
  dplyr::mutate(pub = "akweb", description = NA) %>%
  dplyr::select(pub, link, pubdate, title, description) -> valid_links


saveRDS(valid_links, "akweb.RDS")




