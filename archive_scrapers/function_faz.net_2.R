#devtools::install_github("ropensci/RSelenium")
#install.packages("RSelenium")
require(RSelenium)
require(magrittr)

rD <- RSelenium::rsDriver(browser = "firefox", 
                          #chromever = "103.0.5060.134", 
                          port = sample(c(5678L, 5679L, 5680L, 5681L, 5682L), size = 1), 
                          #phantomver = "2.1.1",
                          check = FALSE, verbose = FALSE)

remDr <- rD[["client"]]

#binman::list_versions("phantomjs")

# require(webdriver)
# require(magrittr)
# pjs_instance <- run_phantomjs()
# pjs_session <- Session$new(port = pjs_instance$port)


#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")


#function for geting links from page
faz_get_links <- function(html){
  
  #html <- remDr$getPageSource()[[1]]
  #html <- pjs_session$getSource()
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'Teaser620')]//a//h2") %>% 
    rvest::html_text(trim = TRUE) -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'Teaser620')]/a[1]") %>% 
    rvest::html_attr("href") %>% paste0("https://www.faz.net",.)-> item_link
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//span[contains(@class, 'ThemaAuszeichnung')]") %>% 
    rvest::html_text(trim = TRUE) %>% stringr::str_extract("[0-9]+\\.[0-9]+\\.[0-9]+") %>%
    lubridate::dmy()-> item_pubdate
  
  df <- data.frame(item_title, item_link, item_pubdate)
  return(df)
}


faz_get_url <- function(url){
  remDr$navigate(url)
  print(remDr$getCurrentUrl())
  remDr$getPageSource()[[1]] %>% faz_get_links() -> df
  return(df)
}

faz_go_thr_archive <- function(startdate, enddate){
  #startdate <- "2022-01-01"
  #enddate <- "2022-01-31"
  seq(as.Date(startdate), as.Date(enddate), by="days") %>% 
    format.Date(format="-%Y-%B-%d") -> V1
  
  V1 %>% paste0("https://www.faz.net/artikel-chronik/nachrichten", ., "/") %>%
    purrr::map_df(~faz_get_url(.)) -> df
  
  # pjs_session$go(url)
  # print(url)
  
  return(df)
}

# df <- zeit_getlink_url("https://www.zeit.de/thema/krieg-in-ukraine", "2022-01-01")
  
  

faz_go_thr_archive("2022-01-01", Sys.Date())-> valid_links


valid_links %>% dplyr::rename(title = item_title, link = item_link, pubdate = item_pubdate) %>% 
  dplyr::mutate(pub = "FAZ", description = NA) %>%
  dplyr::select(pub, link, pubdate, title, description) -> valid_links

saveRDS(valid_links, "FAZ.RDS")

remDr$close()
z <- rD$server$stop()

# 