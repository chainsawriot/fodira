#devtools::install_github("ropensci/RSelenium")
#install.packages("RSelenium")
# require(RSelenium)
# require(magrittr)
# 
# rD <- RSelenium::rsDriver(browser = "firefox", 
#                           #chromever = "103.0.5060.134", 
#                           port = sample(c(5678L, 5679L, 5680L, 5681L, 5682L), size = 1), 
#                           #phantomver = "2.1.1",
#                           check = FALSE, verbose = FALSE)
# 
# remDr <- rD[["client"]]

#binman::list_versions("phantomjs")

require(webdriver)
require(magrittr)
pjs_instance <- run_phantomjs()
pjs_session <- Session$new(port = pjs_instance$port)

#pjs_session$go("https://www.hildesheimer-allgemeine.de/suche.html?tx_kesearch_pi1%5Bpage%5D=1")

#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

#writeLines(html, "test.html")

#function for geting links from page
bmopo_get_links <- function(html){
  
  #html <- remDr$getPageSource()[[1]]
  html <- pjs_session$getSource()
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//form//div[contains(@class, 'relative ')]//a//strong") %>% 
    rvest::html_text(trim=TRUE) -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//form//div[contains(@class, 'relative ')]//a") %>% 
    rvest::html_attr("href") %>% paste0("https://www.morgenpost.de",.)-> item_link
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//form//span[contains(@class, 'font-bold')]") %>%
    rvest::html_text(trim = TRUE) %>% stringr::str_extract("[0-9]+\\.[0-9]+\\.[0-9]+") %>%
    lubridate::dmy()-> item_pubdate
  
  
  
  df <- data.frame(item_title, item_link, item_pubdate)
  return(df)
}


bmopo_get_url <- function(url){
  # remDr$navigate(url)
  # print(remDr$getCurrentUrl())
  # remDr$getPageSource()[[1]] %>% tagesspiegel_get_links() -> df
  pjs_session$go(url)
  print(pjs_session$getUrl())
  pjs_session$getSource() %>% bmopo_get_links() -> df
  #print(nrow(df))
  return(df)
}

bmopo_go_thr_archive <- function(startdate, startn, endn){

  
  paste0("https://www.morgenpost.de/suche/?query=Uhr&date=all") %>%
    purrr::map_df(~bmopo_get_url(.)) %>% 
    subset(., item_pubdate >= startdate) -> df
  
  
  nrow(df) -> n
  print(n)
  i <- startn
  while (i <= endn) {
    
    paste0("https://www.morgenpost.de/suche/?query=Uhr&date=all&page=", i) %>%
      purrr::map_df(~bmopo_get_url(.)) %>% 
      subset(., item_pubdate >= startdate) -> df2
    nrow(df2) -> n
    if(n == 0){
      i <- endn + 1
    }
    print(n)
    i <- i+1
    df <- rbind(df, df2)
  }
  
  return(df)
}

# df <- zeit_getlink_url("https://www.zeit.de/thema/krieg-in-ukraine", "2022-01-01")


bmopo_go_thr_archive("2023-01-01", 2, 500) -> valid_links1
bmopo_go_thr_archive("2023-01-01", 501, 1000) -> valid_links2
  
valid_links <- dplyr::distinct(rbind(valid_links1, valid_links2
))

valid_links %>% dplyr::distinct() %>%
  dplyr::rename(title = item_title, link = item_link, pubdate = item_pubdate) %>% 
  dplyr::mutate(pub = "Berliner Morgenpost", description = NA) %>%
  dplyr::select(pub, link, pubdate, title, description) -> valid_links

saveRDS(valid_links, "Berliner Morgenpost.RDS")

 # remDr$close()
 # z <- rD$server$stop()
