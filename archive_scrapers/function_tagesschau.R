
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
  Sys.sleep(4)
  print(url)
  return(tagesschau_getlink(pjs_session$getSource()))
}


tagesschau_go_thr_archive <- function(startdate, enddate){
  V1<-seq(as.Date(startdate), as.Date(enddate), by="days")
  
  V1 %>% as.character() %>%
    paste0("https://www.tagesschau.de/archiv/?datum=", .) %>%
    purrr::map_df(~tagesschau_getlink_url(.)) -> valid_links
  
  return(valid_links)
}


valid_links1 <- tagesschau_go_thr_archive("2021-08-10", "2022-09-01")

valid_links2 <- tagesschau_go_thr_archive("2022-09-01", "2022-11-01")


valid_links3 <- tagesschau_go_thr_archive("2022-11-01", "2023-01-01")

valid_links4 <- tagesschau_go_thr_archive("2023-01-01", Sys.Date())

valid_links <- rbind(valid_links1, valid_links2, valid_links3, valid_links4)

valid_links <- dplyr::distinct(valid_links)

valid_links %>% dplyr::rename(title = item_title, link = item_link, pubdate = item_pubdate) %>% 
  dplyr::mutate(pub = "Tagesschau", description = NA) %>%
  dplyr::select(pub, link, pubdate, title, description) -> valid_links


saveRDS(valid_links, "Tagesschau.RDS")
