
require(webdriver)
require(magrittr)
pjs_instance <- run_phantomjs()
pjs_session <- Session$new(port = pjs_instance$port)


#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

#function for geting links from page
blaetter_getlink <- function(html){

  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//h3/a") %>% 
    rvest::html_text(., trim = TRUE) -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//h3/a") %>% 
    rvest::html_attr("href") %>% paste0("https://www.blaetter.de", .)-> item_link
    
    df <- data.frame(item_title, item_link)
    return(df)
}

blaetter_getlink_url <- function(url){
  pjs_session$go(url)
  print(url)
  return(blaetter_getlink(pjs_session$getSource()))
}

blaetter_go_thr_archive <- function(startdate){
  format(seq(as.Date(startdate), Sys.Date(), by="months"), "%Y/%B") %>%
    stringr::str_replace(., "M.+rz", "Maerz") -> V1
  
  V1 %>% as.character() %>%
    paste0("https://www.blaetter.de/ausgabe/", .) %>%
    purrr::map_df(~blaetter_getlink_url(.)) -> valid_links
  
  return(valid_links)
}

valid_links <- blaetter_go_thr_archive("2021-12-01")

valid_links %>% dplyr::rename(title = item_title, link = item_link) %>% 
  dplyr::mutate(pub = "blaetter.de", description = NA, pubdate = NA) %>%
  dplyr::select(pub, link, pubdate, title, description) -> valid_links


saveRDS(valid_links, "blaetter.de.RDS")



