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

pjs_session$go("https://www.hildesheimer-allgemeine.de/suche.html?tx_kesearch_pi1%5Bpage%5D=1")

#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

writeLines(html, "test.html")

#function for geting links from page
haz_get_links <- function(html){
  
  #html <- remDr$getPageSource()[[1]]
  html <- pjs_session$getSource()
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//h2//a") %>% 
    rvest::html_text(trim = TRUE) -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//h2/a") %>% 
    rvest::html_attr("href") %>% paste0("https://www.hildesheimer-allgemeine.de",.)-> item_link
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//span[contains(@class, 'c-meta__label')]") %>%
    rvest::html_text(trim = TRUE) %>% stringr::str_extract("[0-9]+\\.[0-9]+\\.[0-9]+") %>%
    lubridate::dmy()-> item_pubdate
  
  df <- data.frame(item_title, item_link, item_pubdate)
  return(df)
}


haz_get_url <- function(url){
  # remDr$navigate(url)
  # print(remDr$getCurrentUrl())
  # remDr$getPageSource()[[1]] %>% tagesspiegel_get_links() -> df
  pjs_session$go(url)
  print(pjs_session$getUrl())
  pjs_session$getSource() %>% haz_get_links() -> df
  return(df)
}

haz_go_thr_archive <- function(startdate){

  haz_get_url("https://www.hildesheimer-allgemeine.de/suche.html?tx_kesearch_pi1%5Bpage%5D=1") %>%
    subset(., item_pubdate >= as.Date(startdate))-> df
  n <- nrow(df)
  i <- 2
  print(n)
  while (n>0) {
    haz_get_url(paste0("https://www.hildesheimer-allgemeine.de/suche.html?tx_kesearch_pi1%5Bpage%5D=", i)) %>%
      subset(., item_pubdate >= as.Date(startdate)) -> df2
    n <- nrow(df2)
    print(n)
    i <- i+1
    df <- rbind(df, df2)
  }
  
  return(df)
}

# df <- zeit_getlink_url("https://www.zeit.de/thema/krieg-in-ukraine", "2022-01-01")
  
  
haz_go_thr_archive("2021-12-31") -> valid_links


 # remDr$close()
 # z <- rD$server$stop()
