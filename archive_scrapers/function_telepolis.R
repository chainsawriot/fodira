
require(webdriver)
require(magrittr)
pjs_instance <- run_phantomjs()
pjs_session <- Session$new(port = pjs_instance$port)

#function for geting links from page
#Sys.setlocale("LC_TIME", "C")
#Sys.setlocale("LC_TIME", "de_DE")


telepolis_getlink <- function(html){

    rvest::read_html(html) %>% 
      rvest::html_elements(xpath = "//li[contains(@class, 'archiv-liste__item')]//a") %>% 
    rvest::html_attr("title") -> item_title
    
    rvest::read_html(html) %>% 
      rvest::html_elements(xpath = "//li[contains(@class, 'archiv-liste__item')]//a") %>% 
      rvest::html_attr("href")  %>% paste0("https://www.heise.de", .)-> item_link
    
    rvest::read_html(html) %>% 
      rvest::html_elements(xpath = "//li[contains(@class, 'archiv-liste__item')]//time") %>% 
      rvest::html_attr("datetime") %>% 
      as.Date() -> item_pubdate
    
    df <- subset(data.frame(item_title, item_link, item_pubdate))
    return(df)
}


telepolis_getlink_url <- function(url){
  pjs_session$go(url)
  print(url)
  return(telepolis_getlink(pjs_session$getSource()))
}

#function for going through each archive page

telepolis_go_thr_archive <- function(startdate){
  V1<-seq(as.Date(startdate), Sys.Date(), by="months")
  
  V1 %>% as.character() %>% stringr::str_replace(pattern = "-", replacement = "/") %>%
    stringr::str_remove(pattern = "-[0-9]*") %>%
    paste0("https://www.heise.de/tp/archiv/", ., "/") %>%
    purrr::map_df(~telepolis_getlink_url(.)) -> valid_links
  
  return(valid_links)
}

valid_links <- telepolis_go_thr_archive("2022-08-01")


valid_links %>% dplyr::rename(title = item_title, link = item_link, pubdate = item_pubdate) %>% 
  dplyr::mutate(pub = "Telepolis", description = NA) %>%
  dplyr::select(pub, link, pubdate, title, description) -> valid_links


saveRDS(valid_links, "Telepolis.RDS")




