
require(webdriver)
require(magrittr)
pjs_instance <- run_phantomjs()
pjs_session <- Session$new(port = pjs_instance$port)


#Sys.setlocale("LC_TIME", "C")
#Sys.setlocale("LC_TIME", "de_DE")

#function for geting links from page
jungle_world_getlink <- function(html){

  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//h4[contains(@class, 'public')]//a") %>% 
    rvest::html_text(., trim = TRUE) -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//h4[contains(@class, 'public')]//a") %>% 
    rvest::html_attr("href")  %>% paste0("https://jungle.world", .)-> item_link
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//h5//time") %>% 
    rvest::html_attr("datetime") %>% 
    as.Date() -> item_pubdate
    
    df <- data.frame(item_title, item_link, item_pubdate)
    return(df)
}

jungle_world_getlink_url <- function(url){
  pjs_session$go(url)
  print(url)
  return(jungle_world_getlink(pjs_session$getSource()))
}

jungle_world_go_thr_archive <- function(dates){

  dates %>% as.character() %>%
    paste0("https://jungle.world/inhalt/", .) %>%
    purrr::map_df(~jungle_world_getlink_url(.)) -> valid_links
  
  return(valid_links)
}

valid_links <- jungle_world_go_thr_archive(c("2022/01", "2022/02", "2022/03",
                                             "2022/04", "2022/05", "2022/06",
                                             "2022/07", "2022/08", "2022/09",
                                             paste0("2022/", c(10:33,35:51)),
                                             "2023/01", "2023/02", "2023/03",
                                             "2023/04", "2023/05", "2023/06",
                                             "2023/07", "2023/08", "2023/09",
                                             paste0("2023/", c(10:26))))

valid_links %>% dplyr::rename(title = item_title, link = item_link, pubdate = item_pubdate) %>% 
  dplyr::mutate(pub = "Jungle World", description = NA) %>%
  dplyr::select(pub, link, pubdate, title, description) -> valid_links


saveRDS(valid_links, "Jungle World.RDS")




