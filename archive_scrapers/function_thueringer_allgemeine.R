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
  #html <- pjs_session$getSource()
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//ul//article/a") %>% 
    rvest::html_attr("title") -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//ul//article/a") %>% 
    rvest::html_attr("href") %>% paste0("",.)-> item_link
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//span[contains(@class, 'teaser__date')]") %>%
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

th_allg_go_thr_archive <- function(startdate){

  paste0("https://www.thueringer-allgemeine.de/suche/?q=*&sort=neu&p=1") %>%
    purrr::map_df(~th_allg_get_url(.)) %>% 
    subset(., item_pubdate >= startdate) -> df

  
  nrow(df) -> n
  print(n)
  i <- 2
  while (n>0) {
    paste0("https://www.thueringer-allgemeine.de/suche/?q=*&sort=neu&p=", i) %>%
      purrr::map_df(~th_allg_get_url(.)) %>% 
      subset(., item_pubdate >= startdate) -> df2
    nrow(df2) -> n
    print(n)
    i <- i+1
    df <- rbind(df, df2)
  }

  return(df)
}

# df <- zeit_getlink_url("https://www.zeit.de/thema/krieg-in-ukraine", "2022-01-01")
  
  
th_allg_go_thr_archive("2021-12-31") -> valid_links


 # remDr$close()
 # z <- rD$server$stop()
