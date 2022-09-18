
require(webdriver)
require(magrittr)
pjs_instance <- run_phantomjs()
pjs_session <- Session$new(port = pjs_instance$port)


#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

#function for geting links from page
compact_getlink <- function(html){

  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'content')]//h2/a") %>% 
    rvest::html_text(., trim = TRUE) -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'content')]//h2/a") %>% 
    rvest::html_attr("href")  -> item_link
    
    df <- data.frame(item_title, item_link)
    return(df)
}

compact_getlink_url <- function(url){
  pjs_session$go(url)
  print(url)
  return(compact_getlink(pjs_session$getSource()))
}


compact_go_thr_columns <- function(endnr){
  valid_links <- data.frame()
  for (i in 1:endnr) {
    paste0("https://www.compact-online.de/compact-aktuell/page/", i, "/") %>%
      purrr::map_df(~compact_getlink_url(.)) -> subset_links
    valid_links <- rbind(valid_links, subset_links)
    # k=k+1
    # if (k > 50){
    #   print("wait")
    #   Sys.sleep(90)
    #   k <- 0
    # }
  }
  return(valid_links)
}



compact_go_thr_columns(120) -> valid_links

valid_links %>% dplyr::rename(title = item_title, link = item_link) %>% 
  dplyr::mutate(pub = "Compact", description = NA, pubdate = NA) %>%
  dplyr::select(pub, link, pubdate, title, description) -> valid_links


saveRDS(valid_links, "Compact.RDS")



