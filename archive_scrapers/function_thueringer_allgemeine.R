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

#pjs_session$go("https://www.thueringer-allgemeine.de/suche/?q=*&sort=neu")

#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

#writeLines(html, "test.html")

#function for geting links from page
th_allg_get_links <- function(html){
  #html <- remDr$getPageSource()[[1]]
  html <- pjs_session$getSource()
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//form//div[contains(@class, 'relative ')]//a//strong") %>% 
    rvest::html_text(trim=TRUE) -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//form//div[contains(@class, 'relative ')]//a") %>% 
    rvest::html_attr("href") %>% paste0("https://www.thueringer-allgemeine.de",.)-> item_link
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//form//span[contains(@class, 'font-bold')]") %>%
    rvest::html_text(trim = TRUE) %>% stringr::str_extract("[0-9]+\\.[0-9]+\\.[0-9]+") %>%
    lubridate::dmy()-> item_pubdate
  
  
  
  df <- data.frame(item_title, item_link, item_pubdate)
  return(df)
}


th_allg_get_url <- function(url){
  # remDr$navigate(url)
  # print(remDr$getCurrentUrl())
  # remDr$getPageSource()[[1]] %>% tagesspiegel_get_links() -> df
  pjs_session$go(url)
  print(pjs_session$getUrl())
  pjs_session$getSource() %>% th_allg_get_links() -> df
  #print(nrow(df))
  return(df)
}

th_allg_go_thr_archive <- function(startn, endn){
  
  paste0("https://www.thueringer-allgemeine.de/suche/?query=Uhr&date=all") %>%
    purrr::map_df(~th_allg_get_url(.))  -> df
  
  
  nrow(df) -> n
  print(n)
  i <- startn
  while (i <= endn) {
    
    paste0("https://www.thueringer-allgemeine.de/suche/?query=Uhr&date=all&page=", i) %>%
      purrr::map_df(~th_allg_get_url(.))  -> df2
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
  
  
th_allg_go_thr_archive(1,1000) -> valid_links

valid_links %>% dplyr::rename(title = item_title, link = item_link, pubdate = item_pubdate) %>% 
  dplyr::mutate(pub = "TA", description = NA) %>%
  dplyr::select(pub, link, pubdate, title, description) -> valid_links

saveRDS(valid_links, "TA.RDS")

 # remDr$close()
 # z <- rD$server$stop()
