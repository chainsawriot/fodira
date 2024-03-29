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


#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")


#function for geting links from page
tagesspiegel_get_links <- function(html){
  
  #html <- remDr$getPageSource()[[1]]
  #html <- pjs_session$getSource()
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//h2/a") %>% 
    rvest::html_attr("title") -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//h2/a") %>% 
    rvest::html_attr("href") %>% paste0("https://www.tagesspiegel.de",.)-> item_link
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//h2/a//span[contains(@class, 'hcf-date')]") %>%
    rvest::html_text(trim = TRUE) %>% stringr::str_extract("[0-9]+\\.[0-9]+\\.[0-9]+") %>%
    lubridate::dmy()-> item_pubdate
  
  df <- data.frame(item_title, item_link, item_pubdate)
  return(df)
}


tagesspiegel_get_url <- function(url){
  # remDr$navigate(url)
  # print(remDr$getCurrentUrl())
  # remDr$getPageSource()[[1]] %>% tagesspiegel_get_links() -> df
  pjs_session$go(url)
  print(pjs_session$getUrl())
  pjs_session$getSource() %>% tagesspiegel_get_links() -> df
  return(df)
}

tagesspiegel_go_thr_archive <- function(ressort, startdate){

  tagesspiegel_get_url(paste0("https://www.tagesspiegel.de/suchergebnis/artikel/?p9049616=1&search-ressort=", ressort)) %>%
    subset(., item_pubdate >= as.Date(startdate))-> df
  n <- nrow(df)
  i <- 2
  print(n)
  while (n>0) {
    tagesspiegel_get_url(paste0("https://www.tagesspiegel.de/suchergebnis/artikel/?p9049616=", i,"&search-ressort=", ressort)) %>%
      subset(., item_pubdate >= as.Date(startdate)) -> df2
    n <- nrow(df2)
    print(n)
    i <- i+1
    df <- rbind(df, df2)
  }
  
  return(df)
}

# df <- zeit_getlink_url("https://www.zeit.de/thema/krieg-in-ukraine", "2022-01-01")
  
  
c("2968", "2902", "2876", "23852212", "3002", "2984", "2946", "2894", "2882") %>%
  purrr::map_dfr(~tagesspiegel_go_thr_archive(. , "2021-12-31")) -> valid_links


 # remDr$close()
 # z <- rD$server$stop()
