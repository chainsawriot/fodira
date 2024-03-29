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

#pjs_session$go("https://www.ostsee-zeitung.de/archiv/artikel-03-01-2022/")

#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

#writeLines(html, "test.html")

#function for geting links from page
kiel_get_links <- function(html){
  
  #html <- remDr$getPageSource()[[1]]
  #html <- pjs_session$getSource()
  #pjs_session$getUrl()
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'ArchiveContentstyled__ArchiveContentWrapper')]//a[contains(@class, 'Linkstyled__Link')]//h2") %>% 
    rvest::html_text(trim = TRUE) -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'ArchiveContentstyled__ArchiveContentWrapper')]//a[contains(@class, 'Linkstyled__Link')]") %>% 
    rvest::html_attr("href") %>% paste0("https://www.kn-online.de",.)-> item_link
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'ArchiveContentstyled__ArchiveContentWrapper')]//time") %>%
    rvest::html_attr("datetime") %>%
    lubridate::ymd_hms()-> item_pubdate
  
  df <- data.frame(item_title, item_link, item_pubdate)
  return(df)
}


kiel_get_url <- function(url){
  # remDr$navigate(url)
  # print(remDr$getCurrentUrl())
  # remDr$getPageSource()[[1]] %>% tagesspiegel_get_links() -> df
  pjs_session$go(url)
  pjs_session$getSource() %>% rvest::read_html() %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'ArchiveContentstyled__ArchiveContentWrapper')]//a[contains(@class, 'Linkstyled__Link')]//h2") %>% 
    rvest::html_text(trim = TRUE) -> item_title
  
  while(length(item_title) == 0){
    pjs_session$go(url)
    pjs_session$getSource() %>% rvest::read_html() %>% 
      rvest::html_elements(xpath = "//div[contains(@class, 'ArchiveContentstyled__ArchiveContentWrapper')]//a[contains(@class, 'Linkstyled__Link')]//h2") %>% 
      rvest::html_text(trim = TRUE) -> item_title
  }
  
  print(pjs_session$getUrl())
  pjs_session$getSource() %>% kiel_get_links() -> df
  return(df)
}

kiel_go_thr_archive <- function(startdate, enddate){

  seq(as.Date(startdate), as.Date(enddate), by="days") %>% 
    format.Date(format="-%d-%m-%Y/") -> V1
  
  
  V1 %>%
    paste0("https://www.kn-online.de/archiv/artikel", .) %>%
    purrr::map_df(~kiel_get_url(.)) -> valid_links
  
  return(valid_links)
}

# df <- zeit_getlink_url("https://www.zeit.de/thema/krieg-in-ukraine", "2022-01-01")
  
  
kiel_go_thr_archive("2021-12-01", "2022-05-01") -> valid_links1

kiel_go_thr_archive("2022-05-02", Sys.Date()) -> valid_links2

valid_links <- dplyr::distinct(rbind(valid_links1, valid_links2))

#saveRDS(valid_links1, "lvz_test.RDS")
 # remDr$close()
 # z <- rD$server$stop()

valid_links %>% dplyr::rename(title = item_title, link = item_link, pubdate = item_pubdate) %>% 
  dplyr::mutate(pub = "KN", description = NA) %>%
  dplyr::select(pub, link, pubdate, title, description) -> valid_links

valid_links <- dplyr::distinct(valid_links)

saveRDS(valid_links, "KN.RDS")
