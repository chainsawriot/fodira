
require(webdriver)
require(magrittr)
pjs_instance <- run_phantomjs()
pjs_session <- Session$new(port = pjs_instance$port)


#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

#function for geting links from page
welt_getlink <- function(html){

  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'c-tabs__panel-content')]//h4/a") %>% 
    rvest::html_text(., trim = TRUE) -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'c-tabs__panel-content')]//h4/a") %>% 
    rvest::html_attr("href") %>% paste0("https://www.welt.de", .) -> item_link
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//h2[contains(@class, 'c-tabs__headline')]") %>%
    rvest::html_text(., trim = TRUE) %>% stringr::str_extract(., "[0-9]+[.][0-9]+[.][0-9]+") %>%
    as.Date(tryFormat = c("%d.%m.%Y")) -> item_pubdate

    df <- data.frame(item_title, item_link, item_pubdate[1])
    
    return(df)
}

welt_getlink_url <- function(url){
  pjs_session$go(url)
  print(url)
  Sys.sleep(2)
  return(welt_getlink(pjs_session$getSource()))
}

welt_go_thr_archive <- function(startdate){
  seq(as.Date(startdate), Sys.Date(), by="days") %>% 
    format.Date(format="-%d-%m-%Y") %>% stringr::str_replace_all(., "-0", "-")-> V1
  
  V1 %>%
    paste0("https://www.welt.de/schlagzeilen/nachrichten-vom", ., ".html") %>%
    purrr::map_df(~welt_getlink_url(.)) -> valid_links
  
  return(valid_links)
}


welt_go_thr_archive(startdate = "2022-08-01") -> valid_links


valid_links %>% dplyr::rename(title = item_title, link = item_link, pubdate = item_pubdate.1.) %>% 
  dplyr::mutate(pub = "Welt", description = NA) %>%
  dplyr::select(pub, link, pubdate, title, description) -> valid_links

saveRDS(valid_links, "Welt_3.RDS")
