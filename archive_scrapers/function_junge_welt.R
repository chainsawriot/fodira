
require(webdriver)
require(magrittr)
pjs_instance <- run_phantomjs()
pjs_session <- Session$new(port = pjs_instance$port)

#function for geting links from page

juwelt_getlink <- function(html){

    rvest::read_html(html) %>% 
      rvest::html_elements(xpath = "//main//ul//a") %>% 
      rvest::html_text() -> item_title
    
    rvest::read_html(html) %>% 
      rvest::html_elements(xpath = "//main//ul//a") %>% 
      rvest::html_attr("href")  %>% paste0("https://www.jungewelt.de", .)-> item_link
    
    rvest::read_html(html) %>% 
      rvest::html_elements(xpath = "//span[contains(@class, 'date')]") %>% 
      rvest::html_text() %>% as.Date(tryFormats = c("%d.%m.%Y")) -> item_pubdate
    
    if (length(item_pubdate) > 0) {
      df <- subset(data.frame(item_title, item_link, item_pubdate), stringr::str_detect(item_link, "artikel"))
      return(df)
    } else {
      return(data.frame())
    }
         
}

juwelt_getlink_url <- function(url){
  pjs_session$go(url)
  print(url)
  return(juwelt_getlink(pjs_session$getSource()))
}


#function for going through each archive page

juwelt_go_thr_archive <- function(startdate){
  V1<-seq(as.Date(startdate), Sys.Date(), by="days")
  
  V1 %>% as.character() %>% stringr::str_replace(pattern = "-", replacement = "/") %>%
    paste0("https://www.jungewelt.de/", ., "/index.php") %>%
    purrr::map_df(~juwelt_getlink_url(.)) -> valid_links
  
  return(valid_links)
}

valid_links <- juwelt_go_thr_archive("2021-12-31")

valid_links %>% dplyr::rename(title = item_title, link = item_link, pubdate = item_pubdate) %>% 
  dplyr::mutate(pub = "Junge Welt", description = NA) %>%
  dplyr::select(pub, link, pubdate, title, description) -> valid_links


saveRDS(valid_links, "Junge Welt.RDS")



